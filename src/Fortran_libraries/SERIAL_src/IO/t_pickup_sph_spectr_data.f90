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
!!      subroutine alloc_scale_4_l0(picked)
!!      subroutine alloc_gauss_coef_monitor(gauss)
!!
!!      subroutine dealloc_pick_sph_mode(list_pick)
!!      subroutine dealloc_num_pick_layer(picked)
!!      subroutine dealloc_pick_sph_monitor(picked)
!!      subroutine deallocate_scale_4_l0(picked)
!!      subroutine dealloc_gauss_coef_monitor(gauss)
!!@endverbatim
!!
!!@n @param  my_rank   Process ID
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
!>        Global spherical harmonics ID to evaluate  monitoring spectrum
        integer(kind = kint), allocatable :: idx_gl(:,:)
!>        Local spherical harmonics ID to evaluate  monitoring spectrum
        integer(kind = kint), allocatable :: idx_lc(:)
!
!>        Number of fields for monitoring output
!!         @f$ f(r,\theta,\phi) @f$
        integer (kind=kint) ::  num_field_rj =  0
!>        Total number of component for monitoring spectrum
        integer(kind = kint) :: ntot_comp_rj =  0
!>        Number of component for monitoring spectrum
        integer (kind=kint), allocatable :: istack_comp_rj(:)
!>        Field  address for monitoring of @f$ f(r,j) @f$
        integer (kind=kint), allocatable :: ifield_monitor_rj(:)
!>        monitoring spectrum
        real(kind = kreal), allocatable :: d_rj_gl(:,:)
!>        Localy evaluated  monitoring spectrum
        real(kind = kreal), allocatable :: d_rj_lc(:,:)
!>        Name of  monitoring spectrum
        character(len=kchara), allocatable :: spectr_name(:)
!
!>      Scale factor for vector at l=m=0
        real(kind = kreal), allocatable :: scale_for_zelo(:)
!
!>      Number of modes of Gauss coefficients to be evaluated
!>      Name of Gauss coefficients  (g_{l}^{m} or h_{l}^{m})
        character(len=kchara), allocatable :: gauss_mode_name(:)
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
      subroutine alloc_pick_sph_monitor(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      integer(kind = kint) :: num
!
!
      num = picked%num_sph_mode*picked%num_layer
!
      allocate( picked%idx_gl(picked%num_sph_mode,3) )
      allocate( picked%idx_lc(picked%num_sph_mode) )
      allocate( picked%d_rj_lc(picked%ntot_comp_rj,num) )
      allocate( picked%d_rj_gl(picked%ntot_comp_rj,num) )
      allocate( picked%spectr_name(picked%ntot_comp_rj) )
      allocate( picked%istack_comp_rj(0:picked%num_field_rj) )
      allocate( picked%ifield_monitor_rj(picked%num_field_rj) )
!
      if(picked%num_field_rj .gt. 0) then
        picked%ifield_monitor_rj = 0
        picked%istack_comp_rj =    0
      end if
      if(num .gt. 0) then
        picked%idx_gl = -1
        picked%idx_lc =  0
        picked%d_rj_lc = 0.0d0
        picked%d_rj_gl = 0.0d0
      end if
!
      end subroutine alloc_pick_sph_monitor
!
! -----------------------------------------------------------------------
!
      subroutine alloc_scale_4_l0(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      allocate(picked%scale_for_zelo(picked%num_sph_mode) )
      if(picked%num_sph_mode .gt. 0) picked%scale_for_zelo   = 1.0d0
!
      end subroutine alloc_scale_4_l0
!
! -----------------------------------------------------------------------
!
      subroutine alloc_gauss_coef_monitor(gauss)
!
      type(picked_spectrum_data), intent(inout) :: gauss
!
!
      allocate( gauss%gauss_mode_name(gauss%num_sph_mode) )
!
      end subroutine alloc_gauss_coef_monitor
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
      deallocate(picked%idx_gl, picked%d_rj_gl)
      deallocate(picked%idx_lc, picked%d_rj_lc)
      deallocate(picked%spectr_name)
      deallocate(picked%istack_comp_rj, picked%ifield_monitor_rj)
!
      end subroutine dealloc_pick_sph_monitor
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_scale_4_l0(picked)
!
      type(picked_spectrum_data), intent(inout) :: picked
!
      deallocate(picked%scale_for_zelo)
!
      end subroutine deallocate_scale_4_l0
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_gauss_coef_monitor(gauss)
!
      type(picked_spectrum_data), intent(inout) :: gauss
!
!
      deallocate(gauss%gauss_mode_name)
!
      end subroutine dealloc_gauss_coef_monitor
!
! -----------------------------------------------------------------------
!
      end module t_pickup_sph_spectr_data
