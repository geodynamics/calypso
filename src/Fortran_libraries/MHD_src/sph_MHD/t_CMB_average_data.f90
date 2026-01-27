!>@file   t_CMB_average_data.f90
!!@brief      module t_CMB_average_data
!!
!!@author H. Matsui
!!@date Programmed in Apr., 2022
!
!> @brief  Evaluate CMB average data
!!
!!@verbatim
!!      subroutine alloc_CMB_average_data(num, ave_CMB)
!!      subroutine dealloc_CMB_average_data(ave_CMB)
!!        integer(kind = kint), intent(in) :: num
!!        type(CMB_average_data), intent(inout) :: ave_CMB
!!
!!      subroutine write_CMB_average(i_step, time, sph_params, sph_rj,  &
!!     &                             ave_CMB)
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        integer(kind = kint), intent(in) :: ltr
!!        integer(kind = kint), intent(in) :: nlayer_CMB
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(CMB_average_data), intent(in) :: ave_CMB
!!
!!      subroutine dup_CMB_average_header_to_IO                         &
!!     &         (ltr, nri, nlayer_CMB, ave_CMB, sph_OUT)
!!        integer(kind = kint), intent(in) :: ltr, nri
!!        integer(kind = kint), intent(in) :: nlayer_CMB
!!        type(CMB_average_data), intent(in) :: ave_CMB
!!        type(read_sph_spectr_data), intent(inout) :: sph_OUT
!!@endverbatim
!
      module t_CMB_average_data
!
      use m_precision
      use m_constants
!
      use t_base_field_labels
      use t_read_sph_spectra
!
      implicit none
!
      type CMB_average_data
!>        Integer flag for CMB average data output
        integer(kind = kint) :: iflag_CMB_average = 0
!>        compressed file flag for CMB average
        logical :: flag_gzip_CMB_average = .FALSE.
!>        File prefix for CMB average data
        character(len = kchara)                                         &
     &                 :: CMB_average_file_name = 'CMB_average.dat'
!
!>        Radial address for CMB average
        integer(kind = kint) :: kr_ave_CMB
!>        Radius for CMB average
        real(kind = kreal) :: rave_CMB
!
!>        magnetic energy address
        type(base_field_address) :: imonitor_CMB
!
!>        Truncation degree to evaluate CMB average
        integer(kind = kint) :: num_CMB_ave
!>        Name of each CMB average data
        character(len = kchara), allocatable :: CMB_ave_name(:)
!>        CMB average
        real(kind = kreal), allocatable :: ave_data(:)
      end type CMB_average_data
!
      integer(kind = kint), parameter, private :: id_ave_CMB = 42
!
      type(sph_spectr_head_labels), parameter                           &
     &            :: sph_CMB_average_labels = sph_spectr_head_labels(   &
     &                           hdr_nri = 'radial_layers',             &
     &                           hdr_ltr = 'truncation',                &
     &                           hdr_ICB_id = 'Not_used',               &
     &                           hdr_CMB_id = 'CMB_id',                 &
     &                           hdr_kr_in =  'Not_used',               &
     &                           hdr_r_in =   'Not_used',               &
     &                           hdr_kr_out = 'Upper_boundary_ID',      &
     &                           hdr_r_out =  'Upper_boundary_radius',  &
     &                           hdr_num_field = 'Number_of_field',     &
     &                           hdr_num_comp = 'Number_of_components')
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_CMB_average_data(num, ave_CMB)
!
      integer(kind = kint), intent(in) :: num
      type(CMB_average_data), intent(inout) :: ave_CMB
!
!
      ave_CMB%num_CMB_ave = num
      allocate(ave_CMB%CMB_ave_name(ave_CMB%num_CMB_ave))
      allocate(ave_CMB%ave_data(ave_CMB%num_CMB_ave))
!
      if(ave_CMB%num_CMB_ave .le. 0) return
      ave_CMB%ave_data(1:ave_CMB%num_CMB_ave) = 0.0d0
!
      end subroutine alloc_CMB_average_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_CMB_average_data(ave_CMB)
!
      type(CMB_average_data), intent(inout) :: ave_CMB
!
      deallocate(ave_CMB%ave_data, ave_CMB%CMB_ave_name)
!
      end subroutine dealloc_CMB_average_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_CMB_average(i_step, time, sph_params, sph_rj,    &
     &                             ave_CMB)
!
      use t_spheric_parameter
      use t_spheric_rj_data
      use t_buffer_4_gzip
      use sph_monitor_data_text
      use select_gz_stream_file_IO
      use gz_open_sph_vol_mntr_file
!
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(sph_shell_parameters), intent(in) :: sph_params
      type(sph_rj_grid), intent(in) :: sph_rj
      type(CMB_average_data), intent(in) :: ave_CMB
!
      logical :: flag_gzip_lc
      type(buffer_4_gzip) :: zbuf_d
      type(read_sph_spectr_data) :: sph_OUT_d
!
!
      if(ave_CMB%iflag_CMB_average .le. izero) return
      if(sph_rj%idx_rj_degree_zero .le. 0) return
!
      call dup_CMB_average_header_to_IO                                 &
     &   (sph_params%l_truncation, sph_rj%nidx_rj(1),                   &
     &    sph_params%nlayer_CMB, ave_CMB, sph_OUT_d)
!
      flag_gzip_lc = ave_CMB%flag_gzip_CMB_average
      call sel_open_sph_vol_monitor_file                                &
     &   (id_ave_CMB, ave_CMB%CMB_average_file_name,                    &
     &    sph_CMB_average_labels, sph_OUT_d, zbuf_d, flag_gzip_lc)
      call dealloc_sph_espec_name(sph_OUT_d)
!
      call sel_gz_write_text_stream(flag_gzip_lc, id_ave_CMB,           &
     &    volume_pwr_data_text(i_step, time, ave_CMB%num_CMB_ave,       &
     &                         ave_CMB%ave_data), zbuf_d)
      close(id_ave_CMB)
!
      end subroutine write_CMB_average
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dup_CMB_average_header_to_IO                           &
     &         (ltr, nri, nlayer_CMB, ave_CMB, sph_OUT)
!
      use m_time_labels
!
      integer(kind = kint), intent(in) :: ltr, nri
      integer(kind = kint), intent(in) :: nlayer_CMB
      type(CMB_average_data), intent(in) :: ave_CMB
!
      type(read_sph_spectr_data), intent(inout) :: sph_OUT
!
      integer(kind = kint) :: icou
!
!
      sph_OUT%ltr_sph = ltr
      sph_OUT%nri_sph = nri
      sph_OUT%nri_dat = 1
      sph_OUT%kr_ICB =  0
      sph_OUT%kr_CMB =  nlayer_CMB
      sph_OUT%kr_inner = izero
      sph_OUT%kr_outer = ave_CMB%kr_ave_CMB
      sph_OUT%r_inner =  zero
      sph_OUT%r_outer =  ave_CMB%rave_CMB
!
      sph_OUT%nfield_sph_spec = ave_CMB%num_CMB_ave
      sph_OUT%ntot_sph_spec =   ave_CMB%num_CMB_ave
      sph_OUT%num_time_labels = 2
      call alloc_sph_espec_name(sph_OUT)
!
      sph_OUT%ene_sph_spec_name(1) = fhd_t_step
      sph_OUT%ene_sph_spec_name(2) = fhd_time
      icou = sph_OUT%num_time_labels
      sph_OUT%ene_sph_spec_name(icou+1:icou+ave_CMB%num_CMB_ave)        &
     &                   = ave_CMB%CMB_ave_name(1:ave_CMB%num_CMB_ave)
      sph_OUT%ncomp_sph_spec(1:ave_CMB%num_CMB_ave) = 1
!
      end subroutine dup_CMB_average_header_to_IO
!
! -----------------------------------------------------------------------
!
      end module t_CMB_average_data
