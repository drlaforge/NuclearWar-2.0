#ifndef PKG_MCCP_H__
#define PKG_MCCP_H__ 1

#include "driver.h"

#ifdef USE_MCCP

#include <unistd.h>

#include "typedefs.h"

/* --- Macros --- */

#define COMPRESS_BUF_SIZE 8192

/* --- Prototypes --- */

extern void * zlib_alloc (void *opaque, unsigned int items, unsigned int size);
extern void zlib_free (void *opaque, void *address);
extern Bool start_compress (interactive_t * ip, unsigned char telopt);
extern Bool end_compress (interactive_t * ip, Bool force);

#endif /* USE_MCCP */

#endif /* PKG_MCCP_H__ */
