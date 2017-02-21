/* save this file as lookup3.h */
#ifndef _lookup3_h
#define _lookup3_h
#include <stdint.h>
#include <sys/types.h>
/* only accepts uint32_t aligned arrays of uint32_t */
void hashword2(const uint32_t *key,  /* array of uint32_t */
		   size_t length,	     /* number of uint32_t values */
		   uint32_t *pc,	     /* in: seed1, out: hash1 */
		   uint32_t *pb);	     /* in: seed2, out: hash2 */
/* handles arbitrarily aligned arrays of bytes */
void hashlittle2(const void *key,   /* array of bytes */
		 size_t length,     /* number of bytes */
		 uint32_t *pc,      /* in: seed1, out: hash1 */
		 uint32_t *pb);     /* in: seed2, out: hash2 */
#endif /* _lookup3_h */