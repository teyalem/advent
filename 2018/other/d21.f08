program test
    use, intrinsic :: iso_fortran_env
    implicit none

    integer, parameter :: ilen = 100000
    integer :: r2, r3, i, ns(ilen)

    r3 = 0; i = 0; ns = 0

    do
        r2 = ior(r3, 65536)
        r3 = 7637914

        do
            r3 = iand(r3 + iand(r2, 255), 16777215)
            r3 = iand(r3 * 65899, 16777215)
            if (r2 < 256) exit
            r2 = r2 / 256
        end do

        if (any(ns(1:i) == r3)) then
            exit
        else
            i = i + 1
            ns(i) = r3
        end if
    end do

    print *, ns(1)
    print *, ns(i)
end program
