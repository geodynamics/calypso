!patch_4_psf.f90
!      module patch_4_psf
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine set_psf_type_id(numnod, numele, nnod_4_ele, ie,      &
!!     &          ele_search, mark_ele, c_ref)
!!      subroutine count_num_patch_4_psf(numele, numedge, iedge_4_ele,  &
!!     &          ele_search, mark_ele, id_n_on_e, istack_patch_smp)
!!      subroutine set_patch_4_psf(numele, numedge, iedge_4_ele,        &
!!     &          ele_search, mark_ele, id_n_on_e, npatch_tot,          &
!!     &          istack_patch_smp, ie_patch)
!
!      subroutine renumber_patch_id_psf(npatch_tot, istack_nod_smp,     &
!     &          istack_patch_smp, ie_patch)
!
      module patch_4_psf
!
      use m_precision
      use m_constants
!
      use m_machine_parameter
      use m_psf_case_table
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_psf_type_id(numnod, numele, nnod_4_ele, ie,        &
     &          ele_search, mark_ele, c_ref)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: numnod, numele, nnod_4_ele
      integer(kind = kint), intent(in) :: ie(numele, nnod_4_ele)
      real(kind= kreal), intent(in) :: c_ref(numnod)
      type(sect_search_list), intent(in) :: ele_search
!
      integer(kind = kint), intent(inout)                               &
     &       :: mark_ele(ele_search%num_search)
!
      integer(kind = kint) :: ip, iele, ist, ied, inum
      integer(kind = kint) ::  i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer(kind = kint) :: mk1, mk2, mk3, mk4, mk5, mk6, mk7, mk8
      real(kind= kreal), parameter :: zero = 0.0d0, one = 1.0d0
      real(kind= kreal), parameter :: two = 2.0d0
      integer(kind = kint), parameter :: ione = 1, itwo = 2
!
!
!$omp parallel do private(iele,ist,ied,inum, i1,i2,i3,i4,               &
!$omp&                    i5,i6,i7,i8,mk1,mk2,mk3,mk4,mk5,mk6,mk7,mk8)
      do ip = 1, np_smp
        ist = ele_search%istack_search_smp(ip-1) + 1
        ied = ele_search%istack_search_smp(ip)
        do inum = ist, ied
          iele = ele_search%id_search(inum)
          i1 = ie(iele,1)
          i2 = ie(iele,2)
          i3 = ie(iele,3)
          i4 = ie(iele,4)
          i5 = ie(iele,5)
          i6 = ie(iele,6)
          i7 = ie(iele,7)
          i8 = ie(iele,8)
!
          mk1 = (ione + int( sign(one,c_ref(i1)) )) / itwo
          mk2 = (ione + int( sign(one,c_ref(i2)) )) / itwo
          mk3 = (ione + int( sign(one,c_ref(i3)) )) / itwo
          mk4 = (ione + int( sign(one,c_ref(i4)) )) / itwo
          mk5 = (ione + int( sign(one,c_ref(i5)) )) / itwo
          mk6 = (ione + int( sign(one,c_ref(i6)) )) / itwo
          mk7 = (ione + int( sign(one,c_ref(i7)) )) / itwo
          mk8 = (ione + int( sign(one,c_ref(i8)) )) / itwo
!
          mark_ele(inum) = mk1 + (mk2 + (mk3 + (mk4                     &
     &                  + (mk5 + (mk6 + (mk7 + mk8*itwo)*itwo)*itwo)    &
     &                   *itwo)*itwo)*itwo)*itwo
!
        end do
      end do
!$omp end parallel do
!
      end subroutine set_psf_type_id
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_patch_4_psf(numele, numedge, iedge_4_ele,    &
     &          ele_search, mark_ele, id_n_on_e, istack_patch_smp)
!
      use m_geometry_constants
      use t_psf_geometry_list
!
      type(sect_search_list), intent(in) :: ele_search
      integer(kind = kint), intent(in) :: numele, numedge
      integer(kind = kint), intent(in)                                  &
     &              :: iedge_4_ele(numele,nedge_4_ele)
      integer(kind = kint), intent(in) :: id_n_on_e(numedge)
      integer(kind = kint), intent(in)                                  &
     &              :: mark_ele(ele_search%num_search)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_patch_smp(0:np_smp)
!
      integer(kind = kint) :: npatch_smp(np_smp)
      integer(kind = kint) :: ip, iele, ist, ied, inum, np, icou, n
      integer(kind = kint) :: ie1, ie2, ie3, iedge1, iedge2, iedge3
      integer(kind = kint) :: mark
!
!
      npatch_smp(1:np_smp) = 0
!
!$omp parallel do private(iele,ist,ied,inum,mark,np,icou,n,ie1,ie2,ie3, &
!$omp&         iedge1,iedge2,iedge3)
      do ip = 1, np_smp
        ist = ele_search%istack_search_smp(ip-1) + 1
        ied = ele_search%istack_search_smp(ip)
        do inum = ist, ied
          iele = ele_search%id_search(inum)
          mark = mark_ele(inum)
          np = num_patch(mark)
!
          if (np .gt. 0) then
!
            do n = 1, np
              icou = icou + 1
              ie1 = iedge_4_patch(n,mark,1)
              ie2 = iedge_4_patch(n,mark,2)
              ie3 = iedge_4_patch(n,mark,3)
              iedge1 = abs( iedge_4_ele(iele,ie1) )
              iedge2 = abs( iedge_4_ele(iele,ie2) )
              iedge3 = abs( iedge_4_ele(iele,ie3) )
              if (    id_n_on_e(iedge1).ne.id_n_on_e(iedge2)            &
     &          .and. id_n_on_e(iedge2).ne.id_n_on_e(iedge3)            &
     &          .and. id_n_on_e(iedge3).ne.id_n_on_e(iedge1))           &
     &               npatch_smp(ip) = npatch_smp(ip) + 1
            end do
          end if
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        istack_patch_smp(ip) = istack_patch_smp(ip-1) + npatch_smp(ip)
      end do
!
      end subroutine count_num_patch_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_patch_4_psf(numele, numedge, iedge_4_ele,          &
     &          ele_search, mark_ele, id_n_on_e, npatch_tot,            &
     &          istack_patch_smp, ie_patch)
!
      use m_geometry_constants
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: numele, numedge
      integer(kind = kint), intent(in)                                  &
     &              :: iedge_4_ele(numele,nedge_4_ele)
!
      type(sect_search_list), intent(in) :: ele_search
      integer(kind = kint), intent(in) :: id_n_on_e(numedge)
      integer(kind = kint), intent(in)                                  &
     &              :: mark_ele(ele_search%num_search)
      integer(kind = kint), intent(in) :: npatch_tot
      integer(kind = kint), intent(in)                                  &
     &                    :: istack_patch_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: ie_patch(npatch_tot,3)
!
      integer(kind = kint) :: ip, iele, ist, ied, inum, np, icou, n
      integer(kind = kint) :: ie1, ie2, ie3, iedge1, iedge2, iedge3
      integer(kind = kint) :: mark
!
!
!$omp parallel do                                                       &
!$omp& private(iele,ist,ied,inum,mark,np,icou,n,ie1,ie2,ie3,            &
!$omp&         iedge1,iedge2,iedge3)
      do ip = 1, np_smp
        icou = istack_patch_smp(ip-1)
        ist = ele_search%istack_search_smp(ip-1) + 1
        ied = ele_search%istack_search_smp(ip)
        do inum = ist, ied
          iele = ele_search%id_search(inum)
          mark = mark_ele(inum)
          np = num_patch(mark)
!
          if (np .gt. 0) then
!
            do n = 1, np
              ie1 = iedge_4_patch(n,mark,1)
              ie2 = iedge_4_patch(n,mark,2)
              ie3 = iedge_4_patch(n,mark,3)
              iedge1 = abs( iedge_4_ele(iele,ie1) )
              iedge2 = abs( iedge_4_ele(iele,ie2) )
              iedge3 = abs( iedge_4_ele(iele,ie3) )
              if (    id_n_on_e(iedge1).ne.id_n_on_e(iedge2)            &
     &          .and. id_n_on_e(iedge2).ne.id_n_on_e(iedge3)            &
     &          .and. id_n_on_e(iedge3).ne.id_n_on_e(iedge1)) then
                icou = icou + 1
                ie_patch(icou,1) = id_n_on_e(iedge1)
                ie_patch(icou,2) = id_n_on_e(iedge2)
                ie_patch(icou,3) = id_n_on_e(iedge3)
!                   write(40+my_rank,*) 'iedge_4_ele',                  &
!     &                iele, mark, np, iedge_4_ele(iele,1:12)
!                   write(40+my_rank,*) 'id_n_on_e',                    &
!     &                iele, id_n_on_e( abs(iedge_4_ele(iele,1:12)) )
!                   write(40+my_rank,*) 'iedge_4_patch',                &
!     &                icou, iedge_4_patch(n,mark,1:3)
!                   write(40+my_rank,*)
              end if
            end do
!
          end if
!
        end do
      end do
!$omp end parallel do
!
!      write(40+my_rank,*) 'ie_patch'
!      do inum = 1, icou
!        write(40+my_rank,*) inum, ie_patch(inum,1:3)
!      end do
!      close(40+my_rank)
!
      end subroutine set_patch_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine renumber_patch_id_psf(npatch_tot, istack_nod_smp,      &
     &          istack_patch_smp, ie_patch)
!
      integer(kind = kint), intent(in) :: npatch_tot
      integer(kind = kint), intent(in) :: istack_nod_smp(0:0)
      integer(kind = kint), intent(in)                                  &
     &                    :: istack_patch_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: ie_patch(npatch_tot,3)
!
      integer(kind = kint) :: ip, ist, ied, icou
!
!
!$omp parallel do private(ist,ied,icou)
      do ip = 1, np_smp
        ist = istack_patch_smp(ip-1) + 1
        ied = istack_patch_smp(ip)
        do icou = ist, ied
          ie_patch(icou,1) = ie_patch(icou,1) - istack_nod_smp(0)
          ie_patch(icou,2) = ie_patch(icou,2) - istack_nod_smp(0)
          ie_patch(icou,3) = ie_patch(icou,3) - istack_nod_smp(0)
        end do
      end do
!$omp end parallel do
!
      end subroutine renumber_patch_id_psf
!
!  ---------------------------------------------------------------------
!
      end module patch_4_psf
