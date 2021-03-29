/*	$NetBSD: get.h,v 1.6 2011/08/30 19:49:10 joerg Exp $	*/

/*
 * Copyright (c) 1993-95 Mats O Jansson.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *	$NetBSD: get.h,v 1.6 2011/08/30 19:49:10 joerg Exp $
 *
 */

#ifndef _GET_H_
#define _GET_H_

__BEGIN_DECLS
u_char		mopGetChar(const u_char *, int *);
u_short		mopGetShort(const u_char *, int *);
u_int32_t	mopGetLong(const u_char *, int *);
void		mopGetMulti(const u_char *, int *, u_char *, int);
int		mopGetTrans(const u_char *, int);
void		mopGetHeader(const u_char *, int *, const u_char **, const u_char **,
		    u_short *, int *, int);
u_short		mopGetLength(const u_char *, int);
__END_DECLS

#endif /* _GET_H_ */
