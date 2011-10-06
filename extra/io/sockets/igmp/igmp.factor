! Copyright (C) 2011 Doug Coleman.
! See http://factorcode.org/license.txt for BSD license.
USING: accessors alien.c-types alien.endian kernel sequences ;
IN: io.sockets.igmp

! http://tldp.org/HOWTO/Multicast-HOWTO-6.html
! http://tldp.org/HOWTO/Multicast-HOWTO-7.html
! setsockopt: IP_ADD_MEMBERSHIP
! IP_DROP_MEMBERSHIP
! struct ip_mreq

! IP_MULTICAST_IF
! http://www.cs.unc.edu/~jeffay/dirt/FAQ/comp249-001-F99/mcast-socket.html

BE-PACKED-STRUCT: igmp-header
    { type uchar }
    { code uchar }
    { checksum ushort }
    { group uint } ;
    
CONSTANT: IGMP_HOST_MEMBERSHIP_QUERY        HEX: 11
CONSTANT: IGMP_HOST_MEMBERSHIP_REPORT       HEX: 12
CONSTANT: IGMP_DVMRP                        HEX: 13
CONSTANT: IGMP_PIM                          HEX: 14
CONSTANT: IGMP_TRACE                        HEX: 15
CONSTANT: IGMPV2_HOST_NEW_MEMBERSHIP_REPORT HEX: 16
CONSTANT: IGMP_HOST_LEAVE_MESSAGE           HEX: 17
CONSTANT: IGMPV3_HOST_MEMBERSHIP_REPORT     HEX: 22

CONSTANT: IGMP_DELAYING_MEMBER            HEX: 01
CONSTANT: IGMP_IDLE_MEMBER                HEX: 02
CONSTANT: IGMP_LAZY_MEMBER                HEX: 03
CONSTANT: IGMP_SLEEPING_MEMBER            HEX: 04
CONSTANT: IGMP_AWAKENING_MEMBER           HEX: 05

CONSTANT: IGMP_MINLEN                     8
CONSTANT: IGMP_MAX_HOST_REPORT_DELAY      10
CONSTANT: IGMP_TIMER_SCALE                10 
CONSTANT: IGMP_AGE_THRESHOLD              400

: set-checksum ( igmp-message -- igmp-message' )
    dup >c-ptr sum >>checksum ;

: make-igmp-header ( type code group -- igmp-header )
    igmp-header <struct>
        swap >>group
        swap >>code
        swap >>type ; inline

