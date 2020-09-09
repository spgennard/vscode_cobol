       >>SOURCE FORMAT FREE
*>**
*>  Core library: string
*>
*>  @author Olegs Kunicins
*>  @license LGPL-3.0
*> <br>
*>  This library is free software; you can redistribute it and/or
*>  modify it under the terms of the GNU Lesser General Public
*>  License as published by the Free Software Foundation; either
*>  version 3.0 of the License, or (at your option) any later version.
*>  <br><br>
*>  This library is distributed in the hope that it will be useful,
*>  but WITHOUT ANY WARRANTY; without even the implied warranty of
*>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*>  Lesser General Public License for more details.
*>
*>  You should have received a copy of the GNU Lesser General Public
*>  License along with this library.
*>**

*>*
*> Find the position of the first occurrence of a substring in a string.
*> Case-sensitive.
*>
*> @param l-haystack String to search in
*> @param l-needle String to search for
*> @return Position where the needle exists relative to the beginnning
*> of l-haystack. Returns 0 if not found.
*>*
identification division.
function-id. substr-pos.
environment division.
configuration section.
repository. function length intrinsic.
data division.
working-storage section.
    01 haystack-idx usage index value 1.
    01 needle-idx usage index value 1.
linkage section.
    01 l-haystack pic x any length.
    01 l-needle pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-haystack, l-needle returning l-result.
    initialize haystack-idx, needle-idx, l-result all to value.
    if length(l-haystack) < length(l-needle)
        goback
    end-if.
    perform until haystack-idx > length(l-haystack)
        if l-haystack(haystack-idx:1) = l-needle(needle-idx:1)
           if needle-idx = length(l-needle)
               compute l-result = haystack-idx - needle-idx + 1
               exit perform
           end-if
           set needle-idx up by 1
        else
           initialize needle-idx all to value
        end-if
        set haystack-idx up by 1
    end-perform.
end function substr-pos.


*>*
*> Find the position of the first occurrence of a substring in a string.
*> Case-insensitive.
*>
*> @param l-haystack String to search in
*> @param l-needle String to search for
*> @return Position where the needle exists relative to the beginnning
*> of l-haystack. Returns 0 if not found.
*>*
identification division.
function-id. substr-pos-case.
environment division.
configuration section.
repository.
    function lower-case intrinsic
    function substr-pos.
data division.
working-storage section.
linkage section.
    01 l-haystack pic x any length.
    01 l-needle pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-haystack, l-needle returning l-result.
    move substr-pos(lower-case(l-haystack), lower-case(l-needle)) to l-result.
end function substr-pos-case.

*>*
*> Convert one byte into hexadecimal representation.
*>
*> @param l-byte Byte
*> @return 2 hexadecimal chars
*>*
identification division.
function-id. byte-to-hex.
environment division.
configuration section.
data division.
working-storage section.
    01 CHARS pic x(16) value "0123456789ABCDEF".
    01 ws-remainder binary-char unsigned.
    01 ws-quotient binary-char unsigned.
linkage section.
    01 l-byte usage binary-char unsigned.
    01 l-hex pic x(2).
procedure division using l-byte returning l-hex.
    divide l-byte by 16 giving ws-quotient remainder ws-remainder.
    add 1 to ws-remainder.
    add 1 to ws-quotient.
    move CHARS(ws-remainder:1) to l-hex(2:1).
    move CHARS(ws-quotient:1) to l-hex(1:1).
end function byte-to-hex.

*>*
*> Convert one byte into hexadecimal representation.
*>
*> @param l-hex 2 hexadecimal chars
*> @return Byte
*>*
identification division.
function-id. hex-to-byte.
environment division.
configuration section.
repository.
    function ord upper-case intrinsic.
data division.
working-storage section.
    01 ws-remainder usage binary-char unsigned.
    01 ws-quotient usage binary-char unsigned.
linkage section.
    01 l-hex pic x(2).
    01 l-byte usage binary-char unsigned.
procedure division using l-hex returning l-byte.
    compute ws-quotient = ord(upper-case(l-hex(1:1))) - 49.
    if ws-quotient > 16
        subtract 7 from ws-quotient
    end-if.
    compute ws-remainder = ord(upper-case(l-hex(2:1))) - 49.
    if ws-remainder > 16
        subtract 7 from ws-remainder
    end-if.
    compute l-byte = ws-quotient * 16 + ws-remainder.
end function hex-to-byte.

*>*
*> Count the number of substring occurrences. Case-sensitive.
*>
*> @param l-haystack String to search in
*> @param l-needle String to search for
*> @return Number of occurrences
*>*
identification division.
function-id. substr-count.
environment division.
configuration section.
repository. function length intrinsic.
data division.
working-storage section.
    01 haystack-idx usage index value 1.
    01 needle-idx usage index value 1.
linkage section.
    01 l-haystack pic x any length.
    01 l-needle pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-haystack, l-needle returning l-result.
    initialize haystack-idx, needle-idx, l-result all to value.
    if length(l-haystack) < length(l-needle)
        goback
    end-if.
    perform until haystack-idx > length(l-haystack)
        if l-haystack(haystack-idx:1) = l-needle(needle-idx:1)
           if needle-idx = length(l-needle)
               add 1 to l-result
           end-if
           set needle-idx up by 1
        else
           initialize needle-idx all to value
        end-if
        set haystack-idx up by 1
    end-perform.
end function substr-count.

*>*
*> Count the number of substring occurrences. Case-insensitive.
*>
*> @param l-haystack String to search in
*> @param l-needle String to search for
*> @return Number of occurrences
*>*
identification division.
function-id. substr-count-case.
environment division.
configuration section.
repository.
    function lower-case intrinsic
    function substr-count.
data division.
working-storage section.
linkage section.
    01 l-haystack pic x any length.
    01 l-needle pic x any length.
    01 l-result usage binary-long unsigned value 0.
procedure division using l-haystack, l-needle returning l-result.
    move substr-count(lower-case(l-haystack), lower-case(l-needle)) to l-result.
end function substr-count-case.

*>*
*> Generate SHA3-256 message digest
*>
*> @param l-buffer Input bytes
*> @return 64 hexadecimal chars
*>*
identification division.
function-id. sha3-256.
environment division.
configuration section.
repository.
    function byte-to-hex
    function byte-length intrinsic.
data division.
working-storage section.
    78 RATE value 1088.
    78 CAPACITY value 512.
    78 SUFFIX value x"06".
    01 ws-idx usage index.
    01 ws-hash pic x(32).
linkage section.
    01 l-buffer pic x any length.
    01 l-hex.
        05 hex pic x(2) occurs 32 times.
procedure division using l-buffer returning l-hex.
    call "KECCAK" using
        RATE
        CAPACITY
        l-buffer
        byte-length(l-buffer)
        SUFFIX
        ws-hash
        byte-length(ws-hash).
    perform varying ws-idx from 1 by 1 until ws-idx > byte-length(ws-hash)
        move byte-to-hex(ws-hash(ws-idx:1)) to hex(ws-idx)
    end-perform.
end function sha3-256.

*>*
*> Generate SHA3-512 message digest
*>
*> @param l-buffer Input bytes
*> @return 128 hexadecimal chars
*>*
identification division.
function-id. sha3-512.
environment division.
configuration section.
repository.
    function byte-to-hex
    function byte-length intrinsic.
data division.
working-storage section.
    78 RATE value 576.
    78 CAPACITY value 1024.
    78 SUFFIX value x"06".
    01 ws-idx usage index.
    01 ws-hash pic x(64).
linkage section.
    01 l-buffer pic x any length.
    01 l-hex.
        05 hex pic x(2) occurs 64 times.
procedure division using l-buffer returning l-hex.
    call "KECCAK" using
        RATE
        CAPACITY
        l-buffer
        byte-length(l-buffer)
        SUFFIX
        ws-hash
        byte-length(ws-hash).
    perform varying ws-idx from 1 by 1 until ws-idx > byte-length(ws-hash)
        move byte-to-hex(ws-hash(ws-idx:1)) to hex(ws-idx)
    end-perform.
end function sha3-512.

*>*
*> Convert urlencoded symbol into one byte.
*>
*> @param l-symbol Urlencoded symbol (3 bytes)
*> @return Byte
*>*
identification division.
function-id. urlencoded-to-byte.
environment division.
configuration section.
repository. function hex-to-byte.
data division.
working-storage section.
linkage section.
    01 l-urlencoded.
        05 filler pic x(1).
        88 is-urlencoded value "%".
        05 hex pic x(2).
    01 l-byte usage binary-char unsigned.
procedure division using l-urlencoded returning l-byte.
    initialize l-byte all to value.
    if is-urlencoded
        move hex-to-byte(hex) to l-byte
    end-if.
end function urlencoded-to-byte.

*>*
*> Convert one byte into urlencoded symbol.
*>
*> @param l-byte Byte
*> @return Urlencoded symbol (3 bytes)
*>*
identification division.
function-id. byte-to-urlencoded.
environment division.
configuration section.
repository. function byte-to-hex.
data division.
working-storage section.
linkage section.
    01 l-byte usage binary-char unsigned.
    01 l-urlencoded pic x(3).
procedure division using l-byte returning l-urlencoded.
    move "%" to l-urlencoded(1:1).
    move byte-to-hex(l-byte) to l-urlencoded(2:2).
end function byte-to-urlencoded.

*>*
*> Convert ECB exchange rates in CSV format to the list of currency-rate pairs.
*> https://www.ecb.europa.eu/stats/policy_and_exchange_rates/euro_reference_exchange_rates/html/index.en.html
*>
*> @param l-byte CSV string
*> @return Urlencoded symbol Pointer to the list of 64 [pic x(3), pic 9(7)V9(8)] elements
*>*
identification division.
function-id. csv-ecb-rates.
environment division.
configuration section.
repository. function all intrinsic.
data division.
working-storage section.
    01 ws-header usage binary-char unsigned.
    01 ws-header-idx usage index.
    01 ws-field pic x(32).
    01 ws-csv-pointer usage binary-long unsigned.
    01 ws-field-pointer usage binary-long unsigned.
    01 ws-list.
        05 ws-rates occurs 64 times indexed by ws-rates-idx.
            10 ws-currency pic x(3).
            10 ws-rate pic 9(7)V9(8).
linkage section.
    01 l-csv pic x any length.
    01 l-list.
        05 l-rates usage pointer.
procedure division using l-csv returning l-list.
    set l-rates to address of ws-list.
    move 1 to ws-csv-pointer, ws-field-pointer.
    set ws-rates-idx to 1.
    set ws-header-idx to 0.
    move SPACES to ws-field.
    move 1 to ws-header.
    perform until ws-csv-pointer > byte-length(l-csv)
        evaluate TRUE
        when l-csv(ws-csv-pointer:2) = ", "
            if ws-rates-idx > 1
                if ws-header = 1
                    move ws-field to ws-currency(ws-rates-idx - 1)
                else
                    move ws-field to ws-rate(ws-rates-idx - 1)
                end-if
            end-if
            set ws-rates-idx up by 1
            move SPACES to ws-field
            move 1 to ws-field-pointer
            add 2 to ws-csv-pointer
        when l-csv(ws-csv-pointer:1) = x"0a"
            move 0 to ws-header
            set ws-rates-idx to 1
            add 1 to ws-csv-pointer
        when other
           move l-csv(ws-csv-pointer:1) to ws-field(ws-field-pointer:1)
           add 1 to ws-csv-pointer, ws-field-pointer
        end-evaluate
    end-perform.
end function csv-ecb-rates.
