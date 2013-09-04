!set_nnod_for_element.f90
!     module set_nnod_for_element
!
!> @brief Set number of element from element type ID in mesh data
!
!      Written by H. Matsui on Sep., 2007
!>
!>@n
!>      subroutine set_nnod_for_ele_by_eletype(numele, elmtyp, nodelm,  &
!>     &          ierr)
!>@n  Set number of node for each element by refereing element type ID
!>@n
!>      subroutine set_nnod_4_ele_by_max_connect(numele, nodelm,        &
!>     &          nnod_4_ele)
!>@n  Set number of node for all element
!>         by maximium number of each element
!>@n
!>      subroutine check_wrong_element_list(numnod, numele, nnod_4_ele, &
!>     &          nodelm, ie, ierr)
!>@n Check wroong element list by range of node list
!>
!>@n List of the element type is the following:
!!
!>@n@code
!! +--------------+
!! | ELEMENT-TYPE |
!! +--------------+
!!
!!   1D  : rod          111  1-2
!!                      112  1-3:2
!!   2D  : triangle     211  1-2-3
!!                      212  1-2-3:4-5-6
!!   2D  : quad.        221  1-2-3-4
!!                      222  1-2-3-4:5-6-7-8
!!                      223  1-2-3-4:5-6-7-8:9
!!   3D  : tet.         311  1-2-3-4
!!                      312  1-2-3-4:5-6-7:8-9-10
!!   3D  : prism        321  1-2-3-4-5-6
!!                      322  1-2-3-4-5-6:7-8-9:10-11-12:13-14-15
!!   3D  : hexa.        331  1-2-3-4-5-6-7-8
!!                      332  1-2-3-4-5-6-7-8:9-10-11-12:13-14-15-16
!!                          :17-18-19-20
!!                      333  1-2-3-4-5-6-7-8:9-10-11-12:13-14-15-16
!!                          :17-18-19-20:21-22-23-24-25-26:27
!!
!!   master-slave(tri)  411  1:2-3-4 
!!                      412  1:2-3-4:5-6-7
!!   master-slave(quad) 421  1:2-3-4-5 
!!                      422  1:2-3-4-6:6-7-8-9
!!
!!   joint (tri)        511 (1-2-3)*(4-5-6)
!!                      512 (1-2-3:4-5-6)*(7-8-9:10-11-12)
!!   joint (quad)       521 (1-2-3-4)*(5-6-7-8)
!!                      522 (1-2-3-4:5-6-7-8)*(9-10-11-12:13-14-15-16)
!!
!!   beam               611  1-2
!!                      612  1-2:3
!!
!!   shell: triangle    711  1-2-3
!!                      712  1-2-3:4-5-6
!!   shell: quad.       721  1-2-3-4
!!                      722  1-2-3-4:5-6-7-8
!!===
!>@endcode
!
      module set_nnod_for_element
!
      use m_precision
!
      implicit none
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!
      subroutine set_nnod_for_ele_by_eletype(numele, elmtyp, nodelm,    &
     &          ierr)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: elmtyp(numele)
!
      integer(kind = kint), intent(inout) :: ierr
      integer(kind = kint), intent(inout) :: nodelm(numele)
      integer(kind = kint) :: iele, ityp
!
      ierr = 0
      nodelm(1:numele) = 0
!
      do iele = 1, numele
        ityp = elmtyp(iele)
        if (ityp.eq.111) nodelm(iele)=  2
        if (ityp.eq.112) nodelm(iele)=  3
        if (ityp.eq.211) nodelm(iele)=  3
        if (ityp.eq.212) nodelm(iele)=  6
        if (ityp.eq.221) nodelm(iele)=  4
        if (ityp.eq.222) nodelm(iele)=  8
        if (ityp.eq.223) nodelm(iele)=  9
        if (ityp.eq.311) nodelm(iele)=  4
        if (ityp.eq.312) nodelm(iele)= 10
        if (ityp.eq.321) nodelm(iele)=  6
        if (ityp.eq.322) nodelm(iele)= 15
        if (ityp.eq.331) nodelm(iele)=  8
        if (ityp.eq.332) nodelm(iele)= 20
        if (ityp.eq.333) nodelm(iele)= 27
        if (ityp.eq.411) nodelm(iele)=  4
        if (ityp.eq.412) nodelm(iele)=  7
        if (ityp.eq.421) nodelm(iele)=  5
        if (ityp.eq.422) nodelm(iele)=  9
        if (ityp.eq.511) nodelm(iele)=  6
        if (ityp.eq.512) nodelm(iele)= 12
        if (ityp.eq.521) nodelm(iele)=  8
        if (ityp.eq.522) nodelm(iele)= 16
        if (ityp.eq.611) nodelm(iele)=  2
        if (ityp.eq.612) nodelm(iele)=  3
        if (ityp.eq.711) nodelm(iele)=  3
        if (ityp.eq.712) nodelm(iele)=  6
        if (ityp.eq.721) nodelm(iele)=  4
        if (ityp.eq.722) nodelm(iele)=  8

        if (ityp.le.  0) then
          write(*,*) 'There is no element type for',  iele
          ierr = 1004
          return
        end if
      end do
!
      end subroutine set_nnod_for_ele_by_eletype
!
!   --------------------------------------------------------------------
!
      subroutine set_nnod_4_ele_by_max_connect(numele, nodelm,          &
     &          nnod_4_ele)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: nodelm(numele)
      integer(kind = kint), intent(inout) :: nnod_4_ele
!
      integer(kind = kint) :: iele
!
!
      nnod_4_ele = nodelm(1)
      do iele = 2, numele
        nnod_4_ele = max(nnod_4_ele, nodelm(iele) )
      end do
!
      end subroutine set_nnod_4_ele_by_max_connect
!
!   --------------------------------------------------------------------
!
      subroutine check_wrong_element_list(numnod, numele, nnod_4_ele,   &
     &          nodelm, ie, ierr)
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: nodelm(numele)
      integer(kind = kint), intent(in) :: ie(numele,nnod_4_ele)
      integer(kind = kint), intent(inout) :: ierr
!
      integer(kind = kint) :: iele, inod, k
!
      ierr = 0
      do iele = 1, numele
        if (nodelm(iele).eq.0) then
          write(*,*) 'There is wrong element ID at ',  iele
          ierr = 5000
          return
        end if
!
        do k= 1, nodelm(iele)
          inod = ie(iele,k)
          if (inod.le.0) then
            write(*,*) 'there is no corresponding node for ',  iele
            ierr = 1005
            return
          else if (inod.gt.numnod) then
            write(*,*) 'there is no corresponding node for ',  iele
            ierr = 2001
            return
          end if
        end do
      end do
!
      end subroutine check_wrong_element_list
!
!   --------------------------------------------------------------------
!
      end module set_nnod_for_element
