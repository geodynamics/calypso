!set_element_list_for_psf.f90
!      module set_element_list_for_psf
!
!        programmed by H.Matsui on june, 2006
!
!      subroutine allocate_work_4_mark_psf
!      subroutine deallocate_work_4_mark_psf
!      subroutine mark_element_list_4_psf(numele, interior_ele,         &
!     &        num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp, &
!     &        ngrp_area, id_ele_grp_psf)
!
!!      subroutine count_element_list_4_psf(iele_smp_stack, ele_search)
!!      subroutine set_element_list_4_psf(iele_smp_stack, ele_search)
!
!
      module set_element_list_for_psf
!
      use m_precision
!
      implicit  none
!
      integer(kind = kint), allocatable:: imark_ele(:)
      integer(kind = kint), allocatable:: nele_search_smp(:)
!
      private :: imark_ele, nele_search_smp
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine allocate_work_4_mark_psf(numele)
!
      use m_machine_parameter
!
      integer(kind = kint), intent(in) :: numele
!
!
      allocate(imark_ele(numele))
      allocate(nele_search_smp(np_smp))
!
      imark_ele(1:numele) =       0
      nele_search_smp(1:np_smp) = 0
!
      end subroutine allocate_work_4_mark_psf
!
!  ---------------------------------------------------------------------
!
      subroutine deallocate_work_4_mark_psf
!
      deallocate(imark_ele)
      deallocate(nele_search_smp)
!
      end subroutine deallocate_work_4_mark_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine mark_element_list_4_psf(numele, interior_ele,          &
     &        num_ele_grp, ntot_ele_grp, istack_ele_grp, item_ele_grp,  &
     &        ngrp_area, id_ele_grp_psf)
!
      integer(kind = kint), intent(in) :: numele
      integer(kind = kint), intent(in) :: interior_ele(numele)
      integer(kind = kint), intent(in) :: num_ele_grp, ntot_ele_grp
      integer(kind = kint), intent(in) :: istack_ele_grp(0:num_ele_grp)
      integer(kind = kint), intent(in) :: item_ele_grp(ntot_ele_grp)
!
      integer(kind = kint), intent(in) :: ngrp_area
      integer(kind = kint), intent(in) :: id_ele_grp_psf(ngrp_area)
!
      integer(kind = kint) :: i, inum, iele, igrp, ist, ied
!
!
!$omp parallel do
      do iele = 1, numele
        imark_ele(iele) = 0
      end do
!$omp end parallel do
!
      if (id_ele_grp_psf(1) .eq. 0) then
!$omp parallel do private(iele)
        do iele = 1, numele
          imark_ele(iele) = interior_ele(iele)
        end do
!$omp end parallel do
      else
!
        do i = 1, ngrp_area
          igrp = id_ele_grp_psf(i)
          ist = istack_ele_grp(igrp-1) + 1
          ied = istack_ele_grp(igrp)
!$omp parallel do private(iele)
          do inum = ist, ied
            iele = item_ele_grp(inum)
            imark_ele(iele) = interior_ele(iele)
          end do
!$omp end parallel do
        end do
!
      end if
!
      end subroutine mark_element_list_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine count_element_list_4_psf(iele_smp_stack, ele_search)
!
      use m_machine_parameter
      use t_psf_geometry_list
!
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
      type(sect_search_list), intent(inout) :: ele_search
      integer(kind=kint) :: ip, ist, ied, iele
!
!
      nele_search_smp(1:np_smp) = 0
!$omp parallel do private(iele,ist,ied)
      do ip = 1, np_smp
        ist = iele_smp_stack(ip-1) + 1
        ied = iele_smp_stack(ip)
        do iele = ist, ied
          nele_search_smp(ip) = nele_search_smp(ip) + imark_ele(iele)
        end do
      end do
!$omp end parallel do
!
      do ip = 1, np_smp
        ele_search%istack_search_smp(ip)                                &
     &       = ele_search%istack_search_smp(ip-1) + nele_search_smp(ip)
      end do
      ele_search%num_search = ele_search%istack_search_smp(np_smp)
!
      end subroutine count_element_list_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_element_list_4_psf(iele_smp_stack, ele_search)
!
      use m_machine_parameter
      use t_psf_geometry_list
!
      integer(kind=kint), intent(in) :: iele_smp_stack(0:np_smp)
      type(sect_search_list), intent(inout) :: ele_search
!
      integer(kind = kint) :: ip, iele, ist, ied, icou
!
!$omp parallel do private(iele,ist,ied,icou)
      do ip = 1, np_smp
        icou = ele_search%istack_search_smp(ip-1)
        ist = iele_smp_stack(ip-1) + 1
        ied = iele_smp_stack(ip)
        do iele = ist, ied
          if (imark_ele(iele) .gt. 0) then
            icou = icou + 1
            ele_search%id_search(icou) = iele
          end if
        end do
      end do
!$omp end parallel do
!
      end subroutine set_element_list_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      end module set_element_list_for_psf
