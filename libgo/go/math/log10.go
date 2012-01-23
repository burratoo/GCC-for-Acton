// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package math

// Log10 returns the decimal logarithm of x.
// The special cases are the same as for Log.
func libc_log10(float64) float64 __asm__("log10")
func Log10(x float64) float64 {
	return libc_log10(x)
}

func log10(x float64) float64 {
	return Log(x) * (1 / Ln10)
}

// Log2 returns the binary logarithm of x.
// The special cases are the same as for Log.
func libc_log2(float64) float64 __asm__("log2")
func Log2(x float64) float64 {
	return libc_log2(x)
}

func log2(x float64) float64 {
	return Log(x) * (1 / Ln2)
}
