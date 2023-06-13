!>@file   t_picked_sph_spectr_data_IO.f90
!!@brief  module t_picked_sph_spectr_data_IO
!!
!!@author H. Matsui
!!@date Programmed in Dec., 2012
!
!>@brief  Data arrays to monitoring spectrum data
!!
!!@verbatim
!!      subroutine alloc_pick_sph_monitor_IO(picked_IO)
!!      subroutine alloc_pick_sph_series(n_step, picked_IO)
!!      subroutine dealloc_pick_sph_series(picked_IO)
!!      subroutine dealloc_pick_sph_monitor_IO(picked_IO)
!!        integer(kind = kint), intent(in) :: n_step
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!
!!      subroutine copy_to_pick_sph_series                              &
!!     &         (icou, i_step, time, picked_IO)
!!        integer(kind = kint), intent(in) :: icou
!!        integer(kind = kint), intent(in) :: i_step
!!        real(kind = kreal), intent(in) :: time
!!        type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!!
!!      integer(kind = kint) function get_each_picked_sph_address       &
!!     &                   (id_radius, in_degree, in_order, picked_IO)
!!        integer(kind = kint), intent(in) :: id_radius
!!        integer(kind = kint), intent(in) :: in_degree, in_order
!!        type(picked_spectrum_data_IO), intent(in) :: picked_IO
!!      integer(kind = kint) function get_each_picked_sph_address       &
!!     &                   (id_radius, in_degree, in_order, picked_IO)
!!        integer(kind = kint), intent(in) :: id_radius
!!        integer(kind = kint), intent(in) :: in_degree, in_order
!!        type(picked_spectrum_data_IO), intent(in) :: picked_IO
!!@endverbatim
!!
      module t_picked_sph_spectr_data_IO
!
      use m_precision
      use m_constants
      use t_pickup_sph_spectr_data
!
      implicit  none
!
      type picked_spectrum_data_IO
!>        Number of radial layer for monitoring spectrum
        integer(kind = kint) :: num_layer = 0
!>        Number of modes of  monitoring spectrum to be evaluated
        integer(kind = kint) :: num_mode =  0
!>        Number of fields
        integer(kind = kint) :: num_field = 0
!>        Number of components
        integer(kind = kint) :: ntot_comp = 0
!>        Number of data for each step
        integer(kind = kint) :: ntot_data = 0
!
!>        Global spherical harmonics ID to evaluate  monitoring spectrum
        integer(kind = kint), allocatable :: idx_sph(:,:)
!>        radius
        real(kind = kreal), allocatable :: radius(:)
!>        snapshot of monitoring spectrum
        real(kind = kreal), allocatable :: d_pk(:)
!>        Name of  monitoring spectrum
        character(len=kchara) :: time_labels(6)
!>        Name of  monitoring spectrum
        character(len=kchara), allocatable :: spectr_name(:)
!>        Number of component
        integer(kind=kint), allocatable :: num_comp(:)
!
!>        Number of time series
        integer(kind = kint) :: n_step = 0
!>        Number of data for each step
        integer(kind = kint), allocatable :: i_step(:)
!>        snapshot of monitoring spectrum
        real(kind = kreal), allocatable :: d_time(:)
!>        snapshot of monitoring spectrum
        real(kind = kreal), allocatable :: d_pick(:,:)
      end type picked_spectrum_data_IO
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_monitor_IO(picked_IO)
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
      integer(kind = kint) :: ntot_pick_spectr
!
      ntot_pick_spectr = picked_IO%num_mode * picked_IO%num_layer
      picked_IO%ntot_data = picked_IO%ntot_comp * ntot_pick_spectr
!
      allocate( picked_IO%idx_sph(ntot_pick_spectr,4) )
      allocate( picked_IO%radius(ntot_pick_spectr) )
      allocate( picked_IO%d_pk(picked_IO%ntot_data) )

      allocate(picked_IO%num_comp(picked_IO%num_field))
      allocate(picked_IO%spectr_name(picked_IO%ntot_comp))
!
      if(ntot_pick_spectr .gt. 0) then
        picked_IO%idx_sph = -1
        picked_IO%radius = 0.0d0
        picked_IO%d_pk = 0.0d0
      end if
!
      end subroutine alloc_pick_sph_monitor_IO
!
! -----------------------------------------------------------------------
!
      subroutine alloc_pick_sph_series(n_step, picked_IO)
!
      integer(kind = kint), intent(in) :: n_step
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
!
      picked_IO%n_step = n_step
!
      allocate(picked_IO%i_step(picked_IO%n_step))
      allocate(picked_IO%d_time(picked_IO%n_step))
      allocate(picked_IO%d_pick(picked_IO%ntot_data,picked_IO%n_step))
!
      if(picked_IO%n_step .gt. 0) then
        picked_IO%i_step = -1
        picked_IO%d_time = 0.0d0
        picked_IO%d_pick = 0.0d0
      end if
!
      end subroutine alloc_pick_sph_series
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_series(picked_IO)
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
!
      deallocate(picked_IO%i_step, picked_IO%d_time)
      deallocate(picked_IO%d_pick)
!
      end subroutine dealloc_pick_sph_series
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_pick_sph_monitor_IO(picked_IO)
!
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
!
      deallocate( picked_IO%idx_sph)
      deallocate( picked_IO%radius)
      deallocate( picked_IO%d_pk)

      deallocate(picked_IO%spectr_name,  picked_IO%num_comp)
!
      end subroutine dealloc_pick_sph_monitor_IO
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_to_pick_sph_series                                &
     &         (icou, i_step, time, picked_IO)
!
      integer(kind = kint), intent(in) :: icou
      integer(kind = kint), intent(in) :: i_step
      real(kind = kreal), intent(in) :: time
      type(picked_spectrum_data_IO), intent(inout) :: picked_IO
!
!
      picked_IO%i_step(icou) = i_step
      picked_IO%d_time(icou) = time
!$omp parallel workshare
      picked_IO%d_pick(1:picked_IO%ntot_data,icou)                      &
     &                     = picked_IO%d_pk(1:picked_IO%ntot_data)
!$omp end parallel workshare
!
      end subroutine copy_to_pick_sph_series
!
! -----------------------------------------------------------------------
! -------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &            get_each_picked_fld_address(fld_name, picked_IO)
!
      character(len=kchara), intent(in) :: fld_name
      type(picked_spectrum_data_IO), intent(in) :: picked_IO
!
      integer(kind = kint) :: i
!
      write(*,*) 'fld_name', trim(fld_name)
      get_each_picked_fld_address = 0
      do i = 1, picked_IO%ntot_comp
        if(trim(fld_name) .eq. picked_IO%spectr_name(i)) then
          get_each_picked_fld_address = i
          exit
        end if
      end do
!
      if(get_each_picked_fld_address .le. 0) then
        write(*,*) 'Input field cannot be found.', trim(fld_name)
        return
      end if
      write(*,*) 'get_each_picked_fld_address',                         &
     &                   get_each_picked_fld_address
!
      end function get_each_picked_fld_address
!
! -------------------------------------------------------------------
!
      integer(kind = kint) function get_each_picked_sph_address         &
     &                   (id_radius, in_degree, in_order, picked_IO)
!
      integer(kind = kint), intent(in) :: id_radius
      integer(kind = kint), intent(in) :: in_degree, in_order
      type(picked_spectrum_data_IO), intent(in) :: picked_IO
!
      integer(kind = kint) :: i
!
      get_each_picked_sph_address = 0
      do i = 1, picked_IO%num_layer * picked_IO%num_mode
        if(    id_radius .eq. picked_IO%idx_sph(i,1)                    &
     &   .and. in_degree .eq. picked_IO%idx_sph(i,3)                    &
     &   .and. in_order .eq.  picked_IO%idx_sph(i,4)) then
          get_each_picked_sph_address = i
          exit
        end if
      end do
!
      if(get_each_picked_sph_address .le. 0) then
        write(*,*) 'Input field cannot be found.'
        return
      end if
      write(*,*) 'get_each_picked_sph_address',                         &
     &                   get_each_picked_sph_address

      end function get_each_picked_sph_address
!
! -------------------------------------------------------------------
!
      end module t_picked_sph_spectr_data_IO
