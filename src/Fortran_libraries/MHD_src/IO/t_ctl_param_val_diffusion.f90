!>@file   t_ctl_param_val_diffusion.f90
!!@brief  module t_ctl_param_val_diffusion
!!
!!@author H. Matsui
!!@date Programmed in 2006
!
!> @brief Control parameter for polytrope
!!
!!@verbatim
!!      subroutine dealloc_val_diffuse_parameters(v_diffuse_param)
!!        type(val_diffuse_parameters), intent(inout) :: v_diffuse_param
!!      subroutine set_valuable_diffusion_ctl(my_rank, val_diffuse_c,   &
!!     &          v_diffuse_param, flag_val_diffuse)
!!        integer, intent(in) :: my_rank
!!        type(val_diffuse_ctl), intent(in) :: val_diffuse_c
!!        type(val_diffuse_parameters), intent(inout) :: v_diffuse_param
!!        logical, intent(inout) :: flag_val_diffuse
!!
!!      subroutine check_val_diffuse_parameters(v_diffuse_param)
!!        type(val_diffuse_parameters), intent(in) :: v_diffuse_param
!!@endverbatim
!
      module t_ctl_param_val_diffusion
!
      use m_precision
      use m_constants
!
!>      Block for polytorope definision
      type val_diffuse_parameters
!>       Stepped variation flag
        logical :: flag_stepped = .FALSE.
!
!>       Density file name
        character(len = kchara) :: diffuse_file_name
!
!>       Number of list
        integer(kind = kint) :: num_diffusion_list = 0
!>       Polytrope index
        real(kind = kreal), allocatable :: diffusion_radius(:)
!>       Polytrope index
        real(kind = kreal), allocatable :: diffusion_list(:)
      end type val_diffuse_parameters
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_val_diffuse_parameters(num, v_diffuse_param)
!
      integer(kind = kint), intent(in) :: num
      type(val_diffuse_parameters), intent(inout) :: v_diffuse_param
!
      v_diffuse_param%num_diffusion_list = num
      allocate(v_diffuse_param%diffusion_radius(num))
      allocate(v_diffuse_param%diffusion_list(num))
!
      if(num .le. 0) return
      v_diffuse_param%diffusion_radius(1:num) = zero
      v_diffuse_param%diffusion_list(1:num) =   zero
!
      end subroutine alloc_val_diffuse_parameters
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_val_diffuse_parameters(v_diffuse_param)
!
      type(val_diffuse_parameters), intent(inout) :: v_diffuse_param
!
      deallocate(v_diffuse_param%diffusion_radius)
      deallocate(v_diffuse_param%diffusion_list)
!
      end subroutine dealloc_val_diffuse_parameters
!
! ----------------------------------------------------------------------
!
      subroutine set_valuable_diffusion_ctl(my_rank, val_diffuse_c,     &
     &          v_diffuse_param, flag_val_diffuse)
!
      use t_ctl_data_valuable_diffuse
      use skip_comment_f
!
      integer, intent(in) :: my_rank
      type(val_diffuse_ctl), intent(in) :: val_diffuse_c
      type(val_diffuse_parameters), intent(inout) :: v_diffuse_param
      logical, intent(inout) :: flag_val_diffuse
!
!
      flag_val_diffuse = .FALSE.
      if(val_diffuse_c%r_variation_ctl%iflag .gt. 0) then
        flag_val_diffuse                                                &
     &             = yes_flag(val_diffuse_c%r_variation_ctl%charavalue)
      end if
      if(flag_val_diffuse .eqv. .FALSE.) return
!
      if(val_diffuse_c%variation_file_name%iflag .gt. 0) then
        v_diffuse_param%diffuse_file_name                               &
     &             = val_diffuse_c%variation_file_name%charavalue
        v_diffuse_param%num_diffusion_list = -1
!
      else if(val_diffuse_c%diffusivity_list_ctl%num .gt. 0) then
        call alloc_val_diffuse_parameters                               &
     &     (val_diffuse_c%diffusivity_list_ctl%num, v_diffuse_param)
!
        do i = 1, v_diffuse_param%num_diffusion_list
          v_diffuse_param%diffusion_radius(i)                           &
     &       = val_diffuse_c%diffusivity_list_ctl%vec1(i)
          v_diffuse_param%diffusion_list(i)                             &
     &       = val_diffuse_c%diffusivity_list_ctl%vec2(i)
        end do
      else
        if(my_rank .eq. 0) write(*,*) 'Set density variation controls'
        flag_val_diffuse = .FALSE.
      end if
!
      end subroutine set_valuable_diffusion_ctl
!
! ----------------------------------------------------------------------
!
      subroutine check_val_diffuse_parameters(v_diffuse_param)
!
      type(val_diffuse_parameters), intent(in) :: v_diffuse_param
!
!
      write(*,*) 'flag_stepped', v_diffuse_param%flag_stepped
!
      write(*,*) 'num_diffusion_list',                                  &
     &          v_diffuse_param%num_diffusion_list
      do i = 1, v_diffuse_param%num_diffusion_list
        write(*,*) i, v_diffuse_param%diffusion_radius(i),              &
     &                v_diffuse_param%diffusion_list(i)
      end do
!
      end subroutine check_val_diffuse_parameters
!
! ----------------------------------------------------------------------
!
      end module t_ctl_param_val_diffusion

