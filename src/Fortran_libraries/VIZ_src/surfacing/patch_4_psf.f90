!>@file   patch_4_psf.f90
!!@brief  module patch_4_psf
!!
!!@date  Programmed by H.Matsui in June, 2006
!
!>@brief Routines to make triangle patich list
!!
!!@verbatim
!!      subroutine set_psf_type_id                                      &
!!     &         (ele, numnod, ele_search, mark_ele, c_ref)
!!      subroutine count_num_patch_4_psf(edge, psf_list,                &
!!     &          ele_search, mark_ele, num_case_tbl, psf_case_tbl,     &
!!     &          istack_patch_smp, ntot_failed)
!!      subroutine set_patch_4_psf(edge, psf_list, ele_search, mark_ele,&
!!     &          num_case_tbl, psf_case_tbl, istack_numele,            &
!!     &          npatch_tot, istack_patch_smp, iele_global, ie_patch)
!!        type(element_data), intent(in) :: ele
!!        type(edge_data), intent(in) :: edge
!!        type(sectioning_list), intent(in) :: psf_list
!!        type(psf_each_case), intent(in) :: psf_case_tbl(num_case_tbl)
!!        type(sect_search_list), intent(in) :: ele_search
!!@endverbatim
!
      module patch_4_psf
!
      use m_precision
      use m_constants
      use m_geometry_constants
!
      use calypso_mpi
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_psf_type_id                                        &
     &         (ele, numnod, ele_search, mark_ele, c_ref)
!
      use t_geometry_data
      use t_psf_geometry_list
!
      type(element_data), intent(in) :: ele
      type(sect_search_list), intent(in) :: ele_search
      integer(kind = kint), intent(in) :: numnod
      real(kind= kreal), intent(in) :: c_ref(numnod)
!
      integer(kind = kint), intent(inout)                               &
     &       :: mark_ele(ele_search%num_search)
!
      integer(kind = kint) :: ip, iele, ist, ied, inum
      integer(kind = kint) ::  i1,  i2,  i3,  i4,  i5,  i6,  i7,  i8
      integer(kind = kint) :: mk1, mk2, mk3, mk4, mk5, mk6, mk7, mk8
!
!
!$omp parallel do private(iele,ist,ied,inum, i1,i2,i3,i4,               &
!$omp&                    i5,i6,i7,i8,mk1,mk2,mk3,mk4,mk5,mk6,mk7,mk8)
      do ip = 1, np_smp
        ist = ele_search%istack_search_smp(ip-1) + 1
        ied = ele_search%istack_search_smp(ip)
        do inum = ist, ied
          iele = ele_search%id_search(inum)
          i1 = ele%ie(iele,1)
          i2 = ele%ie(iele,2)
          i3 = ele%ie(iele,3)
          i4 = ele%ie(iele,4)
          i5 = ele%ie(iele,5)
          i6 = ele%ie(iele,6)
          i7 = ele%ie(iele,7)
          i8 = ele%ie(iele,8)
!
          mk1 = (ione + int( sign(one,c_ref(i1)) ))
          mk2 = (ione + int( sign(one,c_ref(i2)) ))
          mk3 = (ione + int( sign(one,c_ref(i3)) ))
          mk4 = (ione + int( sign(one,c_ref(i4)) ))
          mk5 = (ione + int( sign(one,c_ref(i5)) ))
          mk6 = (ione + int( sign(one,c_ref(i6)) ))
          mk7 = (ione + int( sign(one,c_ref(i7)) ))
          mk8 = (ione + int( sign(one,c_ref(i8)) ))
!
          mark_ele(inum) = mk1 / itwo +  mk2                            &
     &                    + (mk3 + (mk4+ (mk5 + (mk6 + (mk7 + mk8*itwo) &
     &                     *itwo)*itwo)*itwo)*itwo)*itwo
        end do
      end do
!$omp end parallel do
!
      end subroutine set_psf_type_id
!
!  ---------------------------------------------------------------------
!
      subroutine count_num_patch_4_psf(edge, psf_list,                  &
     &          ele_search, mark_ele, num_case_tbl, psf_case_tbl,       &
     &          istack_patch_smp, ntot_failed)
!
      use t_edge_data
      use t_psf_geometry_list
      use t_psf_case_table
!
      type(edge_data), intent(in) :: edge
      type(sectioning_list), intent(in) :: psf_list
!
      integer(kind=kint), intent(in) :: num_case_tbl
      type(psf_each_case), intent(in) :: psf_case_tbl(0:num_case_tbl)
      type(sect_search_list), intent(in) :: ele_search
!
      integer(kind = kint), intent(in)                                  &
     &              :: mark_ele(ele_search%num_search)
!
      integer(kind = kint), intent(inout)                               &
     &              :: istack_patch_smp(0:np_smp)
      integer(kind = kint), intent(inout) :: ntot_failed
!
      integer(kind = kint) :: nfail_smp(np_smp)
      integer(kind = kint) :: npatch_smp(np_smp)
      integer(kind = kint) :: ip, iele, ist, ied, inum, np, n
      integer(kind = kint) :: ie1, ie2, ie3, iedge1, iedge2, iedge3
      integer(kind = kint) :: mark
      integer(kind = kint_gl) ::  ig1, ig2, ig3
!
!
      npatch_smp(1:np_smp) = 0
      nfail_smp(1:np_smp) = 0
      ntot_failed = 0
!
!$omp parallel do private(iele,ist,ied,inum,mark,np,n,ie1,ie2,ie3,      &
!$omp&         iedge1,iedge2,iedge3,ig1,ig2,ig3)
      do ip = 1, np_smp
        ist = ele_search%istack_search_smp(ip-1) + 1
        ied = ele_search%istack_search_smp(ip)
        do inum = ist, ied
          iele = ele_search%id_search(inum)
          mark = mark_ele(inum)
          np = psf_case_tbl(mark)%npatch
!
          if (np .gt. 0) then
!
            do n = 1, np
              ie1 = psf_case_tbl(mark)%iedge(n,1)
              ie2 = psf_case_tbl(mark)%iedge(n,2)
              ie3 = psf_case_tbl(mark)%iedge(n,3)
              iedge1 = abs( edge%iedge_4_ele(iele,ie1) )
              iedge2 = abs( edge%iedge_4_ele(iele,ie2) )
              iedge3 = abs( edge%iedge_4_ele(iele,ie3) )
              ig1 = psf_list%id_n_on_e(iedge1)
              ig2 = psf_list%id_n_on_e(iedge2)
              ig3 = psf_list%id_n_on_e(iedge3)
              if(ig1.gt.0 .and. ig2.gt.0 .and. ig3.gt.0                 &
     &           .and. ig1.ne.ig2  .and. ig2.ne.ig3  .and. ig3.ne.ig1)  &
     &         then
                npatch_smp(ip) = npatch_smp(ip) + 1
              else
                nfail_smp(ip) = nfail_smp(ip) + 1
                if(i_debug .gt. 0) write(my_rank+100,*) 'Failed edge',  &
     &             nfail_smp(ip), iedge1, iedge2, iedge3, ig1, ig2, ig3
              end if
            end do
          end if
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        istack_patch_smp(ip) = istack_patch_smp(ip-1) + npatch_smp(ip)
        ntot_failed = ntot_failed + nfail_smp(ip)
      end do
!
      end subroutine count_num_patch_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_patch_4_psf(edge, psf_list, ele_search, mark_ele,  &
     &          num_case_tbl, psf_case_tbl, istack_numele,              &
     &          npatch_tot, istack_patch_smp, iele_global, ie_patch)
!
      use t_edge_data
      use t_psf_geometry_list
      use t_psf_case_table
!
      integer(kind=kint), intent(in) :: num_case_tbl
      type(edge_data), intent(in) :: edge
      type(sectioning_list), intent(in) :: psf_list
      type(psf_each_case), intent(in) :: psf_case_tbl(0:num_case_tbl)
      type(sect_search_list), intent(in) :: ele_search
!
      integer(kind = kint_gl), intent(in) :: istack_numele
!
      integer(kind = kint), intent(in)                                  &
     &              :: mark_ele(ele_search%num_search)
      integer(kind = kint), intent(in) :: npatch_tot
      integer(kind = kint), intent(in)                                  &
     &                    :: istack_patch_smp(0:np_smp)
!
!
      integer(kind = kint_gl), intent(inout) :: iele_global(npatch_tot)
      integer(kind = kint_gl), intent(inout)                            &
     &                     :: ie_patch(npatch_tot,num_triangle)
!
      integer(kind = kint) :: ip, iele, ist, ied, inum, np, icou, n
      integer(kind = kint) :: ie1, ie2, ie3, iedge1, iedge2, iedge3
      integer(kind = kint) :: imark
      integer(kind = kint_gl) ::  ig1, ig2, ig3
!
!
!$omp parallel do                                                       &
!$omp& private(iele,ist,ied,inum,imark,np,icou,n,ie1,ie2,ie3,           &
!$omp&         iedge1,iedge2,iedge3,ig1,ig2,ig3)
      do ip = 1, np_smp
        icou = istack_patch_smp(ip-1)
        ist = ele_search%istack_search_smp(ip-1) + 1
        ied = ele_search%istack_search_smp(ip)
        do inum = ist, ied
          iele = ele_search%id_search(inum)
          imark = mark_ele(inum)
          np = psf_case_tbl(imark)%npatch
!
          if (np .gt. 0) then
!
            do n = 1, np
              ie1 = psf_case_tbl(imark)%iedge(n,1)
              ie2 = psf_case_tbl(imark)%iedge(n,2)
              ie3 = psf_case_tbl(imark)%iedge(n,3)
              iedge1 = abs( edge%iedge_4_ele(iele,ie1) )
              iedge2 = abs( edge%iedge_4_ele(iele,ie2) )
              iedge3 = abs( edge%iedge_4_ele(iele,ie3) )
              ig1 = psf_list%id_n_on_e(iedge1)
              ig2 = psf_list%id_n_on_e(iedge2)
              ig3 = psf_list%id_n_on_e(iedge3)
              if(ig1.gt.0 .and. ig2.gt.0 .and. ig3.gt.0                 &
     &           .and. ig1.ne.ig2  .and. ig2.ne.ig3  .and. ig3.ne.ig1)  &
     &         then
                icou = icou + 1
                iele_global(icou) = icou + istack_numele
                ie_patch(icou,1) = ig1
                ie_patch(icou,2) = ig2
                ie_patch(icou,3) = ig3
!                write(40+my_rank,*) 'iedge_4_ele',                     &
!     &                iele, imark, np, edge%iedge_4_ele(iele,1:12)
!                write(40+my_rank,*) 'id_n_on_e', iele,                 &
!     &            psf_list%id_n_on_e(abs(edge%iedge_4_ele(iele,1:12)))
!                write(40+my_rank,*) 'iedge_4_patch',                   &
!     &                icou, psf_case_tbl(imark)%iedge(n,1:3)
!                write(40+my_rank,*)
              end if
            end do
!
          end if
!
        end do
      end do
!$omp end parallel do
!
!      write(*,*) 'ie_patch', my_rank, npatch_tot
!      write(40+my_rank,*) 'ie_patch', npatch_tot
!      do inum = 1, npatch_tot
!        write(40+my_rank,*) inum, ie_patch(inum,1:3)
!      end do
!      close(40+my_rank)
!
      end subroutine set_patch_4_psf
!
!  ---------------------------------------------------------------------
!
      end module patch_4_psf
