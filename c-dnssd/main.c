//if (!arg.ei_type == ERL_TUPLE || arg.arity != 2) goto badarg;
//[> decode type <]
//ei_decode_ei_term(buf, &index, &type);
//if (type.ei_type != ERL_BINARY_EXT) goto badarg;
//index += 5; // skip tag + 4 byte size
//type_tmp = (char*)driver_alloc(type.size + 1);
//memset(type_tmp, 0, type.size + 1);
//memcpy(type_tmp, buf + index, type.size);
//index += type.size;
//[> decode domain <]
//ei_decode_ei_term(buf, &index, &domain);
//if (domain.ei_type != ERL_BINARY_EXT) {
//  driver_free(type_tmp);
//  goto badarg;
//}
//index += 5; // skip tag + 4 byte size
//domain_tmp = (char *) driver_alloc(domain.size + 1);
//memset(domain_tmp, 0, domain.size + 1);
//memcpy(domain_tmp, buf + index, domain.size);
//err = DNSServiceBrowse(&dd->sd_ref,
//           0, // Flags
//           kDNSServiceInterfaceIndexAny,
//           type_tmp,
//           domain_tmp,
//           (DNSServiceBrowseReply) BrowseReply,
//           dd);
//driver_free(type_tmp);
//driver_free(domain_tmp);

#include <assert.h>
#include <dns_sd.h>
#include <stdio.h>
#include <string.h>

static void DNSSD_API BrowseReply(DNSServiceRef sd_ref,
                                  DNSServiceFlags flags,
                                  uint32_t ifIndex,
                                  DNSServiceErrorType err,
                                  const char * name,
                                  const char * regtype,
                                  const char * domain,
                                  void * context);

static void DNSSD_API QueryRecordReply(DNSServiceRef sdRef,
                                       DNSServiceFlags flags,
                                       uint32_t interfaceIndex,
                                       DNSServiceErrorType errorCode,
                                       const char *fullname,
                                       uint16_t rrtype,
                                       uint16_t rrclass,
                                       uint16_t rdlen,
                                       const void *rdata,
                                       uint32_t ttl,
                                       void *context);

int main(int argc, const char *argv[])
{
    DNSServiceRef sd_ref = NULL;
    DNSServiceErrorType err;
    //char *type = "_http._tcp";
    //char *domain = "local";

    printf("browsing...\n");

    err = DNSServiceBrowse(&sd_ref,
                           0, // Flags
                           kDNSServiceInterfaceIndexAny,
                           "_http._tcp",
                           "",
                           BrowseReply,
                           NULL);
    printf("browse: %s\n", ( err == kDNSServiceErr_NoError
                             ? "ok"
                             : "error" ));

    err = DNSServiceProcessResult(sd_ref);
    printf("process: %s\n", ( err == kDNSServiceErr_NoError
                              ? "ok"
                              : "error" ));

    DNSServiceRefDeallocate(sd_ref);
    sd_ref = NULL;

    printf("\n");
    printf("querying for txt...\n");

    err = DNSServiceQueryRecord(&sd_ref,
                                0, // flags
                                kDNSServiceInterfaceIndexAny,
                                "x4._http._tcp.local",
                                kDNSServiceType_TXT,
                                kDNSServiceClass_IN,
                                (DNSServiceQueryRecordReply) QueryRecordReply,
                                NULL);
    printf("query txt: %s\n", ( err == kDNSServiceErr_NoError
                                ? "ok"
                                : "error" ));

    err = DNSServiceProcessResult(sd_ref);
    printf("process: %s\n", ( err == kDNSServiceErr_NoError
                              ? "ok"
                              : "error" ));

    return 0;
}

static void DNSSD_API BrowseReply(DNSServiceRef sd_ref,
                                  DNSServiceFlags flags,
                                  uint32_t ifIndex,
                                  DNSServiceErrorType err,
                                  const char * name,
                                  const char * regtype,
                                  const char * domain,
                                  void * context)
{
    if (err != kDNSServiceErr_NoError) {
        printf("error: %d\n", err);
        return;
    }
    printf("name: %s regtype: %s domain: %s\n", name, regtype, domain);
}

static void DNSSD_API QueryRecordReply(DNSServiceRef sdRef,
                                       DNSServiceFlags flags,
                                       uint32_t interfaceIndex,
                                       DNSServiceErrorType err,
                                       const char *fullname,
                                       uint16_t rrtype,
                                       uint16_t rrclass,
                                       uint16_t rdlen,
                                       const void *rdata,
                                       uint32_t ttl,
                                       void *context)
{
//#define NOT_IMPLEMENTED false
//    assert(NOT_IMPLEMENTED);
    if (err != kDNSServiceErr_NoError) {
        printf("error: %d\n", err);
        return;
    }
#define SIZE 2048
    char rdata_tmp[SIZE];
    strlcpy(rdata_tmp, rdata, rdlen < SIZE ? rdlen+1 : SIZE);
#undef SIZE
    printf("name: %s rrtype: %s rdata: %s\n",
           fullname,
           rrtype == kDNSServiceType_TXT ? "txt" : "?",
           rdata_tmp);
}
