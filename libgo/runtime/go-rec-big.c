/* go-rec-big.c -- receive something larger than 64 bits on a channel.

   Copyright 2009 The Go Authors. All rights reserved.
   Use of this source code is governed by a BSD-style
   license that can be found in the LICENSE file.  */

#include <stdint.h>

#include "go-panic.h"
#include "channel.h"

/* Returns true if a value was received, false if the channel is
   closed.  */

_Bool
__go_receive_big (struct __go_channel *channel, void *val, _Bool for_select)
{
  uintptr_t element_size;
  size_t alloc_size;
  size_t offset;

  if (channel == NULL)
    __go_panic_msg ("receive from nil channel");

  element_size = channel->element_type->__size;
  alloc_size = (element_size + sizeof (uint64_t) - 1) / sizeof (uint64_t);

  if (!__go_receive_acquire (channel, for_select))
    {
      __builtin_memset (val, 0, element_size);
      return 0;
    }

  offset = channel->next_fetch * alloc_size;
  __builtin_memcpy (val, &channel->data[offset], element_size);

  __go_receive_release (channel);

  return 1;
}
