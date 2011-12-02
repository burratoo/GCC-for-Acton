/* Generic streaming support for various data types.

   Copyright 2011 Free Software Foundation, Inc.
   Contributed by Diego Novillo <dnovillo@google.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_DATA_STREAMER_H
#define GCC_DATA_STREAMER_H

#include "vec.h"
#include "lto-streamer.h"

/* Data structures used to pack values and bitflags into a vector of
   words.  Used to stream values of a fixed number of bits in a space
   efficient way.  */
static unsigned const BITS_PER_BITPACK_WORD = HOST_BITS_PER_WIDE_INT;

typedef unsigned HOST_WIDE_INT bitpack_word_t;
DEF_VEC_I(bitpack_word_t);
DEF_VEC_ALLOC_I(bitpack_word_t, heap);

struct bitpack_d
{
  /* The position of the first unused or unconsumed bit in the word.  */
  unsigned pos;

  /* The current word we are (un)packing.  */
  bitpack_word_t word;

  /* The lto_output_stream or the lto_input_block we are streaming to/from.  */
  void *stream;
};


/* String hashing.  */
struct string_slot
{
  const char *s;
  int len;
  unsigned int slot_num;
};


/* Returns a hash code for P.  Adapted from libiberty's htab_hash_string
   to support strings that may not end in '\0'.  */

static inline hashval_t
hash_string_slot_node (const void *p)
{
  const struct string_slot *ds = (const struct string_slot *) p;
  hashval_t r = ds->len;
  int i;

  for (i = 0; i < ds->len; i++)
     r = r * 67 + (unsigned)ds->s[i] - 113;
  return r;
}

/* Returns nonzero if P1 and P2 are equal.  */

static inline int
eq_string_slot_node (const void *p1, const void *p2)
{
  const struct string_slot *ds1 = (const struct string_slot *) p1;
  const struct string_slot *ds2 = (const struct string_slot *) p2;

  if (ds1->len == ds2->len)
    return memcmp (ds1->s, ds2->s, ds1->len) == 0;

  return 0;
}

/* Returns a new bit-packing context for bit-packing into S.  */
static inline struct bitpack_d
bitpack_create (struct lto_output_stream *s)
{
  struct bitpack_d bp;
  bp.pos = 0;
  bp.word = 0;
  bp.stream = (void *)s;
  return bp;
}

/* Pack the NBITS bit sized value VAL into the bit-packing context BP.  */
static inline void
bp_pack_value (struct bitpack_d *bp, bitpack_word_t val, unsigned nbits)
{
  bitpack_word_t word = bp->word;
  int pos = bp->pos;

  /* Verify that VAL fits in the NBITS.  */
  gcc_checking_assert (nbits == BITS_PER_BITPACK_WORD
		       || !(val & ~(((bitpack_word_t)1<<nbits)-1)));

  /* If val does not fit into the current bitpack word switch to the
     next one.  */
  if (pos + nbits > BITS_PER_BITPACK_WORD)
    {
      lto_output_uleb128_stream ((struct lto_output_stream *) bp->stream, word);
      word = val;
      pos = nbits;
    }
  else
    {
      word |= val << pos;
      pos += nbits;
    }
  bp->word = word;
  bp->pos = pos;
}

/* Finishes bit-packing of BP.  */
static inline void
lto_output_bitpack (struct bitpack_d *bp)
{
  lto_output_uleb128_stream ((struct lto_output_stream *) bp->stream,
			     bp->word);
  bp->word = 0;
  bp->pos = 0;
}

/* Returns a new bit-packing context for bit-unpacking from IB.  */
static inline struct bitpack_d
lto_input_bitpack (struct lto_input_block *ib)
{
  struct bitpack_d bp;
  bp.word = lto_input_uleb128 (ib);
  bp.pos = 0;
  bp.stream = (void *)ib;
  return bp;
}

/* Unpacks NBITS bits from the bit-packing context BP and returns them.  */
static inline bitpack_word_t
bp_unpack_value (struct bitpack_d *bp, unsigned nbits)
{
  bitpack_word_t mask, val;
  int pos = bp->pos;

  mask = (nbits == BITS_PER_BITPACK_WORD
	  ? (bitpack_word_t) -1
	  : ((bitpack_word_t) 1 << nbits) - 1);

  /* If there are not continuous nbits in the current bitpack word
     switch to the next one.  */
  if (pos + nbits > BITS_PER_BITPACK_WORD)
    {
      bp->word = val = lto_input_uleb128 ((struct lto_input_block *)bp->stream);
      bp->pos = nbits;
      return val & mask;
    }
  val = bp->word;
  val >>= pos;
  bp->pos = pos + nbits;

  return val & mask;
}


/* Write a character to the output block.  */

static inline void
lto_output_1_stream (struct lto_output_stream *obs, char c)
{
  /* No space left.  */
  if (obs->left_in_block == 0)
    lto_append_block (obs);

  /* Write the actual character.  */
  *obs->current_pointer = c;
  obs->current_pointer++;
  obs->total_size++;
  obs->left_in_block--;
}


/* Read byte from the input block.  */

static inline unsigned char
lto_input_1_unsigned (struct lto_input_block *ib)
{
  if (ib->p >= ib->len)
    lto_section_overrun (ib);
  return (ib->data[ib->p++]);
}

/* Output VAL into OBS and verify it is in range MIN...MAX that is supposed
   to be compile time constant.
   Be host independent, limit range to 31bits.  */

static inline void
lto_output_int_in_range (struct lto_output_stream *obs,
			 HOST_WIDE_INT min,
			 HOST_WIDE_INT max,
			 HOST_WIDE_INT val)
{
  HOST_WIDE_INT range = max - min;

  gcc_checking_assert (val >= min && val <= max && range > 0
		       && range < 0x7fffffff);

  val -= min;
  lto_output_1_stream (obs, val & 255);
  if (range >= 0xff)
    lto_output_1_stream (obs, (val >> 8) & 255);
  if (range >= 0xffff)
    lto_output_1_stream (obs, (val >> 16) & 255);
  if (range >= 0xffffff)
    lto_output_1_stream (obs, (val >> 24) & 255);
}

/* Input VAL into OBS and verify it is in range MIN...MAX that is supposed
   to be compile time constant.  PURPOSE is used for error reporting.  */

static inline HOST_WIDE_INT
lto_input_int_in_range (struct lto_input_block *ib,
			const char *purpose,
			HOST_WIDE_INT min,
			HOST_WIDE_INT max)
{
  HOST_WIDE_INT range = max - min;
  HOST_WIDE_INT val = lto_input_1_unsigned (ib);

  gcc_checking_assert (range > 0 && range < 0x7fffffff);

  if (range >= 0xff)
    val |= ((HOST_WIDE_INT)lto_input_1_unsigned (ib)) << 8;
  if (range >= 0xffff)
    val |= ((HOST_WIDE_INT)lto_input_1_unsigned (ib)) << 16;
  if (range >= 0xffffff)
    val |= ((HOST_WIDE_INT)lto_input_1_unsigned (ib)) << 24;
  val += min;
  if (val < min || val > max)
    lto_value_range_error (purpose, val, min, max);
  return val;
}

/* Output VAL into BP and verify it is in range MIN...MAX that is supposed
   to be compile time constant.
   Be host independent, limit range to 31bits.  */

static inline void
bp_pack_int_in_range (struct bitpack_d *bp,
		      HOST_WIDE_INT min,
		      HOST_WIDE_INT max,
		      HOST_WIDE_INT val)
{
  HOST_WIDE_INT range = max - min;
  int nbits = floor_log2 (range) + 1;

  gcc_checking_assert (val >= min && val <= max && range > 0
		       && range < 0x7fffffff);

  val -= min;
  bp_pack_value (bp, val, nbits);
}

/* Input VAL into BP and verify it is in range MIN...MAX that is supposed
   to be compile time constant.  PURPOSE is used for error reporting.  */

static inline HOST_WIDE_INT
bp_unpack_int_in_range (struct bitpack_d *bp,
		        const char *purpose,
		        HOST_WIDE_INT min,
		        HOST_WIDE_INT max)
{
  HOST_WIDE_INT range = max - min;
  int nbits = floor_log2 (range) + 1;
  HOST_WIDE_INT val = bp_unpack_value (bp, nbits);

  gcc_checking_assert (range > 0 && range < 0x7fffffff);

  if (val < min || val > max)
    lto_value_range_error (purpose, val, min, max);
  return val;
}

/* Output VAL of type "enum enum_name" into OBS.
   Assume range 0...ENUM_LAST - 1.  */
#define lto_output_enum(obs,enum_name,enum_last,val) \
  lto_output_int_in_range ((obs), 0, (int)(enum_last) - 1, (int)(val))

/* Input enum of type "enum enum_name" from IB.
   Assume range 0...ENUM_LAST - 1.  */
#define lto_input_enum(ib,enum_name,enum_last) \
  (enum enum_name)lto_input_int_in_range ((ib), #enum_name, 0, \
					  (int)(enum_last) - 1)

/* Output VAL of type "enum enum_name" into BP.
   Assume range 0...ENUM_LAST - 1.  */
#define bp_pack_enum(bp,enum_name,enum_last,val) \
  bp_pack_int_in_range ((bp), 0, (int)(enum_last) - 1, (int)(val))

/* Input enum of type "enum enum_name" from BP.
   Assume range 0...ENUM_LAST - 1.  */
#define bp_unpack_enum(bp,enum_name,enum_last) \
  (enum enum_name)bp_unpack_int_in_range ((bp), #enum_name, 0, \
					(int)(enum_last) - 1)

/* Output the start of a record with TAG to output block OB.  */

static inline void
output_record_start (struct output_block *ob, enum LTO_tags tag)
{
  lto_output_enum (ob->main_stream, LTO_tags, LTO_NUM_TAGS, tag);
}

/* Return the next tag in the input block IB.  */

static inline enum LTO_tags
input_record_start (struct lto_input_block *ib)
{
  return lto_input_enum (ib, LTO_tags, LTO_NUM_TAGS);
}

/* In data-streamer.c  */
void bp_pack_var_len_unsigned (struct bitpack_d *, unsigned HOST_WIDE_INT);
void bp_pack_var_len_int (struct bitpack_d *, HOST_WIDE_INT);
unsigned HOST_WIDE_INT bp_unpack_var_len_unsigned (struct bitpack_d *);
HOST_WIDE_INT bp_unpack_var_len_int (struct bitpack_d *);

/* In data-streamer-out.c  */
void output_zero (struct output_block *);
void output_uleb128 (struct output_block *, unsigned HOST_WIDE_INT);
void output_sleb128 (struct output_block *, HOST_WIDE_INT);
void lto_output_string (struct output_block *, struct lto_output_stream *,
			const char *, bool);
unsigned lto_string_index (struct output_block *, const char *, unsigned int,
			   bool);
void lto_output_string_with_length (struct output_block *,
				    struct lto_output_stream *,
				    const char *, unsigned int, bool);
const char *input_string_internal (struct data_in *, struct lto_input_block *,
				   unsigned int *);

/* In data-streamer-in.c  */
const char *string_for_index (struct data_in *, unsigned int, unsigned int *);
const char *lto_input_string (struct data_in *, struct lto_input_block *);

#endif  /* GCC_DATA_STREAMER_H  */
