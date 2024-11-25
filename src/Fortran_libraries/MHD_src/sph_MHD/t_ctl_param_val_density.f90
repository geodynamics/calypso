!>@file   t_ctl_param_val_density.f90
!!@brief  module t_ctl_param_val_density
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief Control parameter for polytrope
!!
!!@verbatim
!!      subroutine dealloc_density_variation_list(polytrope_param)
!!        type(polytrope_parameters), intent(inout) :: polytrope_param
!!      subroutine set_valuable_density_ctl(my_rank, polytrope_c,       &
!!     &          polytrope_param, flag_ref_density_valiation)
!!        integer, intent(in) :: my_rank
!!        type(val_density_ctl), intent(in) :: polytrope_c
!!        type(polytrope_parameters), intent(inout) :: polytrope_param
!!        logical, intent(inout) :: flag_ref_density_valiation
!!
!!      subroutine check_polytrope_parameters(polytrope_param)
!!        type(polytrope_parameters), intent(in) :: polytrope_param
!!@endverbatim
!
      module t_ctl_param_val_density
!
      use m_precision
      use m_constants
!
!>      Block for polytorope definision
      type polytrope_parameters
!>       Stepped variation flag
        logical :: flag_stepped = .FALSE.
!
!>       Density file name
        character(len = kchara) :: density_file_name
!
!>       Radius and Density at inner boundary
        real(kind = kreal) :: rho_bottom(2) = (/zero, one/)
!>       Radius and Density at outer boundary
        real(kind = kreal) :: rho_top(2) =    (/one, one/)
!>       Polytrope index
        real(kind = kreal) :: polytrope_idx = zero
!
!>       Number of list
        integer(kind = kint) :: num_density_list = 0
!>       Polytrope index
        real(kind = kreal), allocatable :: density_radius(:)
!>       Polytrope index
        real(kind = kreal), allocatable :: density_list(:)
      end type polytrope_parameters
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_density_variation_list(num, polytrope_param)
!
      integer(kind = kint), intent(in) :: num
      type(polytrope_parameters), intent(inout) :: polytrope_param
!
      polytrope_param%num_density_list = num
      allocate(polytrope_param%density_radius(num))
      allocate(polytrope_param%density_list(num))
!
      if(num .le. 0) return
      polytrope_param%density_radius(1:num) = zero
      polytrope_param%density_list(1:num) = zero
!
      end subroutine alloc_density_variation_list
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_density_variation_list(polytrope_param)
!
      type(polytrope_parameters), intent(inout) :: polytrope_param
!
      deallocate(polytrope_param%density_radius)
      deallocate(polytrope_param%density_list)
!
      end subroutine dealloc_density_variation_list
!
! ----------------------------------------------------------------------
!
      subroutine set_valuable_density_ctl(my_rank, polytrope_c,         &
     &          polytrope_param, flag_ref_density_valiation)
!
      use t_ctl_data_valuable_density
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(val_density_ctl), intent(in) :: polytrope_c
      type(polytrope_parameters), intent(inout) :: polytrope_param
      logical, intent(inout) :: flag_ref_density_valiation
!
!
      flag_ref_density_valiation = .FALSE.
      if(polytrope_c%r_variation_ctl%iflag .gt. 0) then
        flag_ref_density_valiation                                      &
     &             = yes_flag(polytrope_c%r_variation_ctl%charavalue)
      end if
      if(flag_ref_density_valiation .eqv. .FALSE.) return
!
      iflag =  polytrope_c%bottom_density_ctl%iflag                     &
     &       * polytrope_c%top_density_ctl%iflag                        &
     &       * polytrope_c%polytrope_index_ctl%iflag
      if(iflag .gt. 0) then
        polytrope_param%rho_bottom(1:2)                                 &
     &            = polytrope_c%bottom_density_ctl%realvalue(1:2)
        polytrope_param%rho_top(1:2)                                    &
     &            = polytrope_c%top_density_ctl%realvalue(1:2)
        polytrope_param%polytrope_idx                                   &
     &            = polytrope_c%polytrope_index_ctl%realvalue
!
      else if(polytrope_c%variation_file_name%iflag .gt. 0) then
        polytrope_param%density_file_name                               &
     &             = polytrope_c%variation_file_name%charavalue
        polytrope_param%num_density_list = -1
!
      else if(polytrope_c%density_list_ctl%num .gt. 0) then
        call alloc_density_variation_list                               &
     &     (polytrope_c%density_list_ctl%num, polytrope_param)
!
        do i = 1, polytrope_param%num_density_list
          polytrope_param%density_radius(i)                             &
     &       = polytrope_c%density_list_ctl%vec1(i)
          polytrope_param%density_list(i)                               &
     &       = polytrope_c%density_list_ctl%vec2(i)
        end do
      else
        if(my_rank .eq. 0) write(*,*) 'Set density variation controls'
        flag_ref_density_valiation = .FALSE.
      end if
!
      end subroutine set_valuable_density_ctl
!
! ----------------------------------------------------------------------
!
      subroutine check_polytrope_parameters(polytrope_param)
!
      type(polytrope_parameters), intent(in) :: polytrope_param
!
!
      write(*,*) 'flag_stepped', polytrope_param%flag_stepped
      write(*,*) 'rho_bottom (r, rho)', polytrope_param%rho_bottom(1:2)
      write(*,*) 'rho_top (r, rho)',    polytrope_param%rho_top(1:2)
      write(*,*) 'polytrope_idx',       polytrope_param%polytrope_idx
!
      write(*,*) 'num_density_list',    polytrope_param%num_density_list
      do i = 1, polytrope_param%num_density_list
        write(*,*) i, polytrope_param%density_radius(i),                &
     &                polytrope_param%density_list(i)
      end do
!
      end subroutine check_polytrope_parameters
!
! ----------------------------------------------------------------------
!
      end module t_ctl_param_val_density

