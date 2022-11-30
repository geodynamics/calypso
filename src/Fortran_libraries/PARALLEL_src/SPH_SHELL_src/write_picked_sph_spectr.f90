!>@file   write_picked_sph_spectr.f90
!!@brief  module write_picked_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine write_picked_spectrum_files                          &
!!     &         (time_d, sph_params, sph_rj, rj_fld, picked)
!!      integer(kind = kint) function                                   &
!!     &                    error_picked_spectr_files(sph_params, picked)
!!        type(time_data), intent(in) :: time_d
!!        type(sph_shell_parameters), intent(in) :: sph_params
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(picked_spectrum_data), intent(in) :: picked
!!@endverbatim
!!
      module write_picked_sph_spectr
!
      use m_precision
      use m_constants
      use calypso_mpi
!
      use t_spheric_parameter
      use t_pickup_sph_spectr_data
      use t_phys_data
      use t_time_data
      use t_buffer_4_gzip
!
      implicit  none
!
      integer(kind = kint), parameter, private :: id_pick = 17
!
! -----------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine write_picked_spectrum_files                            &
     &         (time_d, sph_params, sph_rj, rj_fld, picked)
!
      use pickup_sph_spectr_data
      use sph_monitor_data_text
      use select_gz_stream_file_IO
      use write_pick_sph_spectr_data
      use write_each_pick_spectr_file
!
      type(time_data), intent(in) :: time_d
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(phys_data), intent(in) :: rj_fld
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: inum, knum
      integer(kind = kint_gl) :: num
!
      real(kind=kreal), allocatable :: d_rj_out(:,:)
      type(buffer_4_gzip) :: zbuf_p
      logical :: flag_gzip_lc
!
!
      if(picked%num_sph_mode_lc .le. 0) return
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .le. 0) return
!
      allocate(d_rj_out(picked%ntot_comp_rj,picked%num_layer))
!
      if(picked%idx_out(0,4) .gt. 0) then
        call pick_center_spectrum_monitor                               &
     &     (rj_fld, picked, picked%ntot_comp_rj, d_rj_out(1,1))
!
        flag_gzip_lc = picked%flag_gzip
        call open_each_picked_spectr(izero, id_pick,                    &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      flag_gzip_lc, zbuf_p)
        call sel_gz_write_text_stream(flag_gzip_lc, id_pick,            &
     &      picked_each_mode_data_text(time_d%i_time_step, time_d%time, &
     &                                 zero, izero, izero, izero,       &
     &                                 picked%ntot_comp_rj,             &
     &                                 d_rj_out(1,1)),                  &
     &      zbuf_p)
        close(id_pick)
      end if
!
      do inum = 1, picked%num_sph_mode_lc
        do knum = 1, picked%num_layer
          call pick_single_sph_spec_4_monitor(inum, knum, sph_rj,       &
     &        rj_fld, picked, picked%ntot_comp_rj, d_rj_out(1,knum))
        end do
!
        call open_each_picked_spectr(inum, id_pick,                     &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      flag_gzip_lc, zbuf_p)
!
        call sel_gz_write_picked_spec_data(flag_gzip_lc, id_pick,       &
     &      time_d, picked, inum, d_rj_out, zbuf_p)
        close(id_pick)
      end do
      deallocate(d_rj_out)
!
      end subroutine write_picked_spectrum_files
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    error_picked_spectr_files(sph_params, picked)
!
      use pickup_sph_spectr_data
      use sph_monitor_data_text
      use select_gz_stream_file_IO
      use write_each_pick_spectr_file
!
      type(sph_shell_parameters), intent(in) :: sph_params
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint) :: inum, num, ierr_lc
!
      type(buffer_4_gzip) :: zbuf_p
!
!
      error_picked_spectr_files = 0
      if(picked%num_sph_mode_lc .le. 0) return
!
      num = picked%istack_picked_spec_lc(my_rank+1)                     &
     &     - picked%istack_picked_spec_lc(my_rank)
      if(num .le. 0) return
!
      ierr_lc = 0
      if(picked%idx_out(0,4) .gt. 0) then
        if(error_each_picked_spectr(izero, id_pick,                     &
     &     sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,        &
     &     zbuf_p))  ierr_lc = ierr_lc + 1
      end if
!
      do inum = 1, picked%num_sph_mode_lc
        if(error_each_picked_spectr(inum, id_pick,                      &
     &      sph_params%nlayer_ICB, sph_params%nlayer_CMB, picked,       &
     &      zbuf_p)) ierr_lc = ierr_lc + 1
      end do
      error_picked_spectr_files = ierr_lc
!
      end function error_picked_spectr_files
!
! -----------------------------------------------------------------------
!
      end module write_picked_sph_spectr
