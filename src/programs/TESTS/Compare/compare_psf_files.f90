!>@file   compare_psf_files.f90
!!@brief  module compare_psf_files
!!
!!@date  Programmed by H.Matsui in Jan., 2021
!
!>@brief control data for cross sections
!
      program compare_psf_files
!
      use m_precision
      use t_ctl_data_psf_compares
      use t_ctl_param_psf_compares
!
      implicit none
!
      type(psf_compare_controls), save :: psf_cmp_list1
!
      integer(kind = kint) :: num_psf_list
      type(psf_compare_param), allocatable, save:: psf_cmp1(:)
!
      integer(kind = kint) :: i, icount_error, icou_error
!
!
      call read_ctl_file_psf_compares(0, psf_cmp_list1)
!
      num_psf_list = psf_cmp_list1%num_psf_cmp
      allocate(psf_cmp1(num_psf_list))
!
      do i = 1, num_psf_list
        call set_control_for_psf_compare(psf_cmp_list1%psf_cmp_ctls(i), &
     &                                   psf_cmp1(i))
      end do
!
      call dealloc_psf_compares_ctl(psf_cmp_list1)
!
      icount_error = 0
      do i = 1, num_psf_list
        call compare_psf_data(psf_cmp1(i), icou_error)
        icount_error = icount_error + icou_error
      end do
      deallocate(psf_cmp1)
!
      end program compare_psf_files
