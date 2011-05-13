// Copyright 2009 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// HTTP Response reading and parsing.

package http

import (
	"bufio"
	"fmt"
	"io"
	"net/textproto"
	"os"
	"sort"
	"strconv"
	"strings"
)

var respExcludeHeader = map[string]bool{
	"Content-Length":    true,
	"Transfer-Encoding": true,
	"Trailer":           true,
}

// Response represents the response from an HTTP request.
//
type Response struct {
	Status     string // e.g. "200 OK"
	StatusCode int    // e.g. 200
	Proto      string // e.g. "HTTP/1.0"
	ProtoMajor int    // e.g. 1
	ProtoMinor int    // e.g. 0

	// RequestMethod records the method used in the HTTP request.
	// Header fields such as Content-Length have method-specific meaning.
	RequestMethod string // e.g. "HEAD", "CONNECT", "GET", etc.

	// Header maps header keys to values.  If the response had multiple
	// headers with the same key, they will be concatenated, with comma
	// delimiters.  (Section 4.2 of RFC 2616 requires that multiple headers
	// be semantically equivalent to a comma-delimited sequence.) Values
	// duplicated by other fields in this struct (e.g., ContentLength) are
	// omitted from Header.
	//
	// Keys in the map are canonicalized (see CanonicalHeaderKey).
	Header Header

	// SetCookie records the Set-Cookie requests sent with the response.
	SetCookie []*Cookie

	// Body represents the response body.
	Body io.ReadCloser

	// ContentLength records the length of the associated content.  The
	// value -1 indicates that the length is unknown.  Unless RequestMethod
	// is "HEAD", values >= 0 indicate that the given number of bytes may
	// be read from Body.
	ContentLength int64

	// Contains transfer encodings from outer-most to inner-most. Value is
	// nil, means that "identity" encoding is used.
	TransferEncoding []string

	// Close records whether the header directed that the connection be
	// closed after reading Body.  The value is advice for clients: neither
	// ReadResponse nor Response.Write ever closes a connection.
	Close bool

	// Trailer maps trailer keys to values, in the same
	// format as the header.
	Trailer Header
}

// ReadResponse reads and returns an HTTP response from r.  The RequestMethod
// parameter specifies the method used in the corresponding request (e.g.,
// "GET", "HEAD").  Clients must call resp.Body.Close when finished reading
// resp.Body.  After that call, clients can inspect resp.Trailer to find
// key/value pairs included in the response trailer.
func ReadResponse(r *bufio.Reader, requestMethod string) (resp *Response, err os.Error) {

	tp := textproto.NewReader(r)
	resp = new(Response)

	resp.RequestMethod = strings.ToUpper(requestMethod)

	// Parse the first line of the response.
	line, err := tp.ReadLine()
	if err != nil {
		if err == os.EOF {
			err = io.ErrUnexpectedEOF
		}
		return nil, err
	}
	f := strings.Split(line, " ", 3)
	if len(f) < 2 {
		return nil, &badStringError{"malformed HTTP response", line}
	}
	reasonPhrase := ""
	if len(f) > 2 {
		reasonPhrase = f[2]
	}
	resp.Status = f[1] + " " + reasonPhrase
	resp.StatusCode, err = strconv.Atoi(f[1])
	if err != nil {
		return nil, &badStringError{"malformed HTTP status code", f[1]}
	}

	resp.Proto = f[0]
	var ok bool
	if resp.ProtoMajor, resp.ProtoMinor, ok = ParseHTTPVersion(resp.Proto); !ok {
		return nil, &badStringError{"malformed HTTP version", resp.Proto}
	}

	// Parse the response headers.
	mimeHeader, err := tp.ReadMIMEHeader()
	if err != nil {
		return nil, err
	}
	resp.Header = Header(mimeHeader)

	fixPragmaCacheControl(resp.Header)

	err = readTransfer(resp, r)
	if err != nil {
		return nil, err
	}

	resp.SetCookie = readSetCookies(resp.Header)

	return resp, nil
}

// RFC2616: Should treat
//	Pragma: no-cache
// like
//	Cache-Control: no-cache
func fixPragmaCacheControl(header Header) {
	if hp, ok := header["Pragma"]; ok && len(hp) > 0 && hp[0] == "no-cache" {
		if _, presentcc := header["Cache-Control"]; !presentcc {
			header["Cache-Control"] = []string{"no-cache"}
		}
	}
}

// ProtoAtLeast returns whether the HTTP protocol used
// in the response is at least major.minor.
func (r *Response) ProtoAtLeast(major, minor int) bool {
	return r.ProtoMajor > major ||
		r.ProtoMajor == major && r.ProtoMinor >= minor
}

// Writes the response (header, body and trailer) in wire format. This method
// consults the following fields of resp:
//
//  StatusCode
//  ProtoMajor
//  ProtoMinor
//  RequestMethod
//  TransferEncoding
//  Trailer
//  Body
//  ContentLength
//  Header, values for non-canonical keys will have unpredictable behavior
//
func (resp *Response) Write(w io.Writer) os.Error {

	// RequestMethod should be upper-case
	resp.RequestMethod = strings.ToUpper(resp.RequestMethod)

	// Status line
	text := resp.Status
	if text == "" {
		var ok bool
		text, ok = statusText[resp.StatusCode]
		if !ok {
			text = "status code " + strconv.Itoa(resp.StatusCode)
		}
	}
	io.WriteString(w, "HTTP/"+strconv.Itoa(resp.ProtoMajor)+".")
	io.WriteString(w, strconv.Itoa(resp.ProtoMinor)+" ")
	io.WriteString(w, strconv.Itoa(resp.StatusCode)+" "+text+"\r\n")

	// Process Body,ContentLength,Close,Trailer
	tw, err := newTransferWriter(resp)
	if err != nil {
		return err
	}
	err = tw.WriteHeader(w)
	if err != nil {
		return err
	}

	// Rest of header
	err = writeSortedHeader(w, resp.Header, respExcludeHeader)
	if err != nil {
		return err
	}

	if err = writeSetCookies(w, resp.SetCookie); err != nil {
		return err
	}

	// End-of-header
	io.WriteString(w, "\r\n")

	// Write body and trailer
	err = tw.WriteBody(w)
	if err != nil {
		return err
	}

	// Success
	return nil
}

func writeSortedHeader(w io.Writer, h Header, exclude map[string]bool) os.Error {
	keys := make([]string, 0, len(h))
	for k := range h {
		if exclude == nil || !exclude[k] {
			keys = append(keys, k)
		}
	}
	sort.SortStrings(keys)
	for _, k := range keys {
		for _, v := range h[k] {
			v = strings.Replace(v, "\n", " ", -1)
			v = strings.Replace(v, "\r", " ", -1)
			v = strings.TrimSpace(v)
			if v == "" {
				continue
			}
			if _, err := fmt.Fprintf(w, "%s: %s\r\n", k, v); err != nil {
				return err
			}
		}
	}
	return nil
}
