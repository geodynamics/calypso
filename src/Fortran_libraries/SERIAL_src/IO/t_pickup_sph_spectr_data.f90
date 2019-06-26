!>@file   t_pickup_sph_spectr_data.f90
!!@brief  module t_pickup_sph_spectr_data
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine alloc_pick_sph_mode(list_pick)
!!      subroutine alloc_pick_sph_l(list_pick)
!!      subroutine alloc_pick_sph_m(list_pick)
!!      subroutine alloc_num_pick_layer(picked)
!!      subroutine alloc_pick_sph_monitor(picked)
!!
!!      subroutine dealloc_pick_sph_mode(list_pick)
!!      subroutine dealloc_num_pick_layer(picked)
!!      subroutine dealloc_pick_sph_monitor(picked)
!!@endverbatim
!!
!!@n @param  i_step    time step
!!@n @param  time      time
!!@n @param  id_pick   file ID
!!@n @param  ierr      Error flag (0:success, 1:error)
!
      module t_pickup_sph_spectr_data
!
      use m_precision
      use m_constants
!
      implicit  none
!
!>        Structure for pickup list
      type pickup_mode_list
!>        Number of modes of monitoring spectrum to be evaluated
        integer(kind = kint) :: num_modes = 0
!>        Degree and Order ID of  monitoring spectrum to be evaluated
        integer(kind = kint), allocatable :: idx_pick_mode(:,:)
!>        Number of degrees of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_degree = 0
!>        Degree ID of  monitoring spectrum to be evaluated
        integer(kind = kint), allocatable :: idx_pick_l(:)
!>        Number of orders of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_order = 0
!>        Order ID of  monitoring spectrum to be evaluated
        integer(kind = kint), allocatable :: idx_pick_m(:)
      end type pickup_mode_list
!
!
!>        Structure for picked spectr data
      type picked_spectrum_data
!>        File prefix for spectr monitoring file
        character(len = kchara) :: file_prefix =  'picked_ene_spec'
!
!>        Number of radial layer for monitoring spectrum
        integer(kind = kint) :: num_layer = 0
!>        Radial ID for monitoring spectrum
        integer(kind = kint), allocatable :: id_radius(:)
!>        Radius for monitoring spectrum
        real(kind = kreal), allocatable :: radius_gl(:)
!
!>        Number of modes of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_sph_mode =  0
!
!>        Number of modes of monitoring spectrum in each process
        integer(kind = kint) :: ntot_pick_spectr_lc = 0
!>        Number of modes of monitoring spectrum in each process
        integer(kind = kint) :: num_sph_mode_lc =  0
!>        Stack of modes for monitoring spectrum
        integer(kind = kint), allocatable :: istack_picked_spec_lc(:)
!>        Stack of modes for monitoring spectrum
!!          (global, l, global m, Global lm, local lm)
        integer(kind = kint), allocatable :: idx_out(:,:)
!>        Name of Gauss coefficients  (g_{l}^{m} or h_{l}^{m})
        character(len=kchara), allocatable :: gauss_mode_name_lc(:)
!
!>        Number of fields for monitoring output
!!         @f$ f(r,\theta,\phi) @f$
        integer(kind = kint) ::  num_field_rj =  0
!>        Total number of component for monitoring spectrum
        integer(kind = kint) :: ntot_comp_rj =  0
!>        Number of component for monitoring spectrum
        integer(kind = kint), allocatable :: istack_comp_rj(:)
!>        Field  address for monitoring of @f$ f(r,j) @f$
        integer(kind = kint), allocatable :: ifield_monitor_rj(:)
!>        Number of modes of monitoring spectrum in each process
        integer(kind = kint) :: ntot_pick_spectr = 0

!>        Name of  monitoring spectrum
        character(len=kchara), allocatable :: spectr_name(:)
      end type picked_spectrum_data
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_mode(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate(list_pick%idx_pick_mode(list_pick%num_modes,2) )
      if(list_pick%num_modes .gt. 0) list_pick%idx_pick_mode = 0
!
      end subroutine alloc_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_l(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate( list_pick%idx_pick_l(list_pick%num_degree) )
      if(list_pick%num_degree .gt. 0) list_pick%idx_pick_l = 0
!
      end subroutine alloc_pick_sph_l
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_m(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      allocate( list_pick%idx_pick_m(list_pick%num_order) )
      if(list_pick%num_order .gt. 0) list_pick%idx_pick_m = 0
!
      end subroutine alloc_pick_sph_m
!
! -----------------------------------------------------------------------
!
      subroutine alloc_num_pick_layer(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      allocate( picked%id_radius(picked%num_layer) )
      allocate( picked%radius_gl(picked%num_layer) )
      if(picked%num_layer .gt. 0) then
        picked%id_radius = 0
        picked%radius_gl = 0.0d0
      end if
!
      end subroutine alloc_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pickup_sph_spec_local(nprocs, picked)
!
      integer, intent(in) :: nprocs
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      picked%ntot_pick_spectr_lc                                        &
     &      = picked%num_sph_mode_lc * picked%num_layer
!
      allocate( picked%istack_picked_spec_lc(0:nprocs) )
!
      allocate( picked%idx_out(0:picked%num_sph_mode_lc,4) )
!
      picked%istack_picked_spec_lc = 0
!$omp parallel workshare
      picked%idx_out =  -1
!$omp end parallel workshare
!
      end subroutine alloc_pickup_sph_spec_local
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pickup_sph_spec_local(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      deallocate( picked%istack_picked_spec_lc)
      deallocate( picked%idx_out)
!
      end subroutine dealloc_pickup_sph_spec_local
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_monitor(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      allocate( picked%spectr_name(picked%ntot_comp_rj) )
      allocate( picked%istack_comp_rj(0:picked%num_field_rj) )
      allocate( picked%ifield_monitor_rj(picked%num_field_rj) )
!
      if(picked%num_field_rj .gt. 0) then
        picked%ifield_monitor_rj = 0
        picked%istack_comp_rj =    0
      end if
!
      end subroutine alloc_pick_sph_monitor
!
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_coef_monitor_lc(gauss)
!
      type(picked_spectrum_data), intent(inout) :: gauss
!
!
      allocate( gauss%gauss_mode_name_lc(gauss%num_sph_mode_lc) )
!
      end subroutine alloc_gauss_coef_monitor_lc
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_coef_monitor_lc(gauss)
!
      type(picked_spectrum_data), intent(inout) :: gauss
!
!
      deallocate( gauss%gauss_mode_name_lc )
!
      end subroutine dealloc_gauss_coef_monitor_lc
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_mode(list_pick)
!
      type(pickup_mode_list), intent(inout) :: list_pick
!
!
      deallocate( list_pick%idx_pick_mode )
      deallocate( list_pick%idx_pick_l, list_pick%idx_pick_m )
!
      end subroutine dealloc_pick_sph_mode
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_num_pick_layer(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      deallocate( picked%id_radius, picked%radius_gl)
!
      end subroutine dealloc_num_pick_layer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_monitor(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
!
      deallocate(picked%spectr_name)
      deallocate(picked%istack_comp_rj, picked%ifield_monitor_rj)
!
      end subroutine dealloc_pick_sph_monitor
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_pick_sph_file_header(id_file, picked)
!
      use m_monitor_file_labels
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      type(picked_spectrum_data), intent(in) :: picked
!
!
      write(id_file,'(a)')    '#'
      write(id_file,'(a)')    '# num_layers, num_spectr'
      write(id_file,'(3i16)') picked%num_layer, picked%num_sph_mode,    &
     &                        picked%ntot_pick_spectr
      write(id_file,'(a)')    '# number of component'
      write(id_file,'(i16)') picked%ntot_comp_rj
!
      write(id_file,'(a)',advance='NO')  't_step    time    '
      write(id_file,'(a)',advance='NO')  'radius_ID    radius    '
      write(id_file,'(a)',advance='NO')  'degree    order    '
!
      call write_multi_labels(id_file, picked%ntot_comp_rj,             &
     &    picked%spectr_name)
!
      end subroutine write_pick_sph_file_header
!
! -----------------------------------------------------------------------
!
      function pick_sph_header_no_field(picked)
!
      use m_monitor_file_labels
!
      type(picked_spectrum_data), intent(in) :: picked
!
      integer(kind = kint), parameter                                   &
     &         :: ilen_h1 = ilen_pick_sph_head + 3*16 + 1
      integer(kind = kint), parameter                                   &
     &         :: ilen_h2 = ilen_pick_sph_num + 16 + 1
      integer(kind = kint), parameter                                   &
     &        :: len_head = ilen_h1 + ilen_h2 + ilen_time_sph_label
!
      character(len = len_head) :: pick_sph_header_no_field
!
!
      write(pick_sph_header_no_field,'(a,3i16,a1,a,i16,a1,a)')          &
     &        hd_pick_sph_head(), picked%num_layer,                     &
     &        picked%num_sph_mode, picked%ntot_pick_spectr, char(10),   &
     &        hd_pick_sph_num(), picked%ntot_comp_rj, char(10),         &
     &        hd_time_sph_label()
!
      end function pick_sph_header_no_field
!
! -----------------------------------------------------------------------
!
      end module t_pickup_sph_spectr_data
