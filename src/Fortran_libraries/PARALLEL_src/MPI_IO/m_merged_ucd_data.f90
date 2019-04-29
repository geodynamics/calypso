!>@file  m_merged_ucd_data.f90
!!       module m_merged_ucd_data
!!
!!@author H. Matsui
!!@date   Programmed by H. Matsui in Feb., 2013
!
!> @brief Data for merged UCD file output
!!
!!@verbatim
!!      subroutine update_ele_by_double_address                         &
!!     &         (istack_internod, dbl_id, m_ucd, ucd)
!!        type(merged_ucd_data), intent(in) :: m_ucd
!!        type(ucd_data), intent(inout) :: ucd
!!        type(parallel_double_numbering), intent(inout) :: dbl_id
!!@endverbatim
!
      module m_merged_ucd_data
!
      use m_precision
      use m_constants
      use t_para_double_numbering
!
      use calypso_mpi
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine update_ele_by_double_address                           &
     &         (istack_internod, dbl_id, m_ucd, ucd)
!
      use t_ucd_data
!
      integer(kind = kint_gl), intent(in) :: istack_internod(0:nprocs)
      type(parallel_double_numbering), intent(in) :: dbl_id
      type(merged_ucd_data), intent(in) :: m_ucd
      type(ucd_data), intent(inout) :: ucd
!
      integer(kind = kint) :: ip, irank, k1
      integer(kind = kint_gl) :: inod, iele
!
      do ip = 1, nprocs
        if(m_ucd%istack_merged_intnod(ip-1)                             &
     &      .ne. istack_internod(ip-1)) then
           write(*,*) 'aho', ip, m_ucd%istack_merged_intnod(ip-1) ,     &
     &                           istack_internod(ip-1)
        end if
      end do
!
!$omp parallel private(iele)
      do k1 = 1, ucd%nnod_4_ele
!$omp do private(inod,irank)
        do iele = 1, ucd%nele
          inod = ucd%ie(iele,k1)
          irank = dbl_id%irank_home(inod)
          ucd%ie(iele,k1) = m_ucd%istack_merged_intnod(irank)           &
     &                 + dbl_id%inod_local(inod)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine update_ele_by_double_address
!
! -----------------------------------------------------------------------
!
      end module m_merged_ucd_data
