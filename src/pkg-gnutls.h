#ifndef PKG_GNUTLS_H__
#define PKG_GNUTLS_H__ 1

#include "driver.h"

#if defined(USE_TLS) && defined(HAS_GNUTLS)
#  include <gnutls/gnutls.h>

#include "typedefs.h"

/* Number of bits for the Diffie Hellmann parameters in OUTGOING
 * connections. Deprecated and will be overridden by priority. */
#define DH_BITS 2048

/* --- Types --- */

typedef gnutls_session_t tls_session_t;

#endif /* USE_TLS && !HAS_OPENSSL */

#endif /* PKG_GNUTLS_H__ */

