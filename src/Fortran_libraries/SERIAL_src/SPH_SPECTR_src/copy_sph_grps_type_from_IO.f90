!>@file   copy_sph_grps_type_from_IO.f90
!!@brief  module copy_sph_grps_type_from_IO
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief Copy sphectr groups from IO buffer
!!
!!@verbatim
!!      subroutine copy_rtp_nod_grp_type_from_IO(sph_grps)
!!      subroutine copy_rtp_r_grp_type_from_IO(sph_grps)
!!      subroutine copy_rtp_theta_grp_type_from_IO(sph_grps)
!!      subroutine copy_rtp_zonal_grp_type_from_IO(sph_grps)
!!      subroutine copy_rj_radial_grp_type_from_IO(sph_grps)
!!      subroutine copy_rj_sphere_grp_type_from_IO(sph_grps)
!!        type(sph_group_data), intent(inout) ::  sph_grps
!!@endverbatim
!
      module copy_sph_grps_type_from_IO
!
      use m_precision
!
      use m_constants
      use m_group_data_sph_specr_IO
      use t_spheric_mesh
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_nod_grp_type_from_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      sph_grps%bc_rtp_grp%num_grp =  num_bc_grp_rtp_IO
      call allocate_grp_type_num(sph_grps%bc_rtp_grp)
!
      if (sph_grps%bc_rtp_grp%num_grp .gt. izero) then
!
        sph_grps%bc_rtp_grp%istack_grp(0) = istack_bc_grp_rtp_IO(0)
        do i = 1, sph_grps%bc_rtp_grp%num_grp
          sph_grps%bc_rtp_grp%grp_name(i) =   name_bc_grp_rtp_IO(i)
          sph_grps%bc_rtp_grp%istack_grp(i) = istack_bc_grp_rtp_IO(i)
        end do
!
        call allocate_grp_type_item(sph_grps%bc_rtp_grp)
        do i = 1, sph_grps%bc_rtp_grp%num_item
          sph_grps%bc_rtp_grp%item_grp(i) = item_bc_grp_rtp_IO(i)
        end do
      else
        sph_grps%bc_rtp_grp%num_item = 0
        call allocate_grp_type_item(sph_grps%bc_rtp_grp)
      end if
!
      call deallocate_rtp_nod_grp_IO_item
!
      end subroutine copy_rtp_nod_grp_type_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_r_grp_type_from_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      sph_grps%radial_rtp_grp%num_grp =  num_radial_grp_rtp_IO
      call allocate_grp_type_num(sph_grps%radial_rtp_grp)
!
      if (sph_grps%radial_rtp_grp%num_grp .gt. izero) then
!
        sph_grps%radial_rtp_grp%istack_grp(0)                           &
     &        = istack_radial_grp_rtp_IO(0)
        do i = 1, sph_grps%radial_rtp_grp%num_grp
          sph_grps%radial_rtp_grp%grp_name(i)                           &
     &        =   name_radial_grp_rtp_IO(i)
          sph_grps%radial_rtp_grp%istack_grp(i)                         &
     &        = istack_radial_grp_rtp_IO(i)
        end do
!
        call allocate_grp_type_item(sph_grps%radial_rtp_grp)
        do i = 1, sph_grps%radial_rtp_grp%num_item
          sph_grps%radial_rtp_grp%item_grp(i)                           &
     &        = item_radial_grp_rtp_IO(i)
        end do
!
      else
        sph_grps%radial_rtp_grp%num_item = 0
        call allocate_grp_type_item(sph_grps%radial_rtp_grp)
      end if
!
      call deallocate_rtp_r_grp_IO_item
!
      end subroutine copy_rtp_r_grp_type_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_theta_grp_type_from_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      sph_grps%theta_rtp_grp%num_grp =  num_theta_grp_rtp_IO
      call allocate_grp_type_num(sph_grps%theta_rtp_grp)
!
      if (sph_grps%theta_rtp_grp%num_grp .gt. izero) then
!
        sph_grps%theta_rtp_grp%istack_grp(0)                            &
     &        = istack_theta_grp_rtp_IO(0)
        do i = 1, sph_grps%theta_rtp_grp%num_grp
          sph_grps%theta_rtp_grp%grp_name(i) = name_theta_grp_rtp_IO(i)
          sph_grps%theta_rtp_grp%istack_grp(i)                          &
     &        = istack_theta_grp_rtp_IO(i)
        end do
!
        call allocate_grp_type_item(sph_grps%theta_rtp_grp)
        do i = 1, sph_grps%theta_rtp_grp%num_item
          sph_grps%theta_rtp_grp%item_grp(i) = item_theta_grp_rtp_IO(i)
        end do
      else
        sph_grps%theta_rtp_grp%num_item = 0
        call allocate_grp_type_item(sph_grps%theta_rtp_grp)
      end if
!
      call deallocate_rtp_t_grp_IO_item
!
      end subroutine copy_rtp_theta_grp_type_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_zonal_grp_type_from_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      sph_grps%zonal_rtp_grp%num_grp =  num_zonal_grp_rtp_IO
      call allocate_grp_type_num(sph_grps%zonal_rtp_grp)
!
      if (sph_grps%zonal_rtp_grp%num_grp .gt. izero) then
!
        sph_grps%zonal_rtp_grp%istack_grp(0)                            &
     &        = istack_zonal_grp_rtp_IO(0)
        do i = 1, sph_grps%zonal_rtp_grp%num_grp
          sph_grps%zonal_rtp_grp%grp_name(i) = name_zonal_grp_rtp_IO(i)
          sph_grps%zonal_rtp_grp%istack_grp(i)                          &
     &        = istack_zonal_grp_rtp_IO(i)
        end do
!
        call allocate_grp_type_item(sph_grps%zonal_rtp_grp)
        do i = 1, sph_grps%zonal_rtp_grp%num_item
          sph_grps%zonal_rtp_grp%item_grp(i) = item_zonal_grp_rtp_IO(i)
        end do
      else
        sph_grps%zonal_rtp_grp%num_item = 0
        call allocate_grp_type_item(sph_grps%zonal_rtp_grp)
      end if
!
      call deallocate_rtp_p_grp_IO_item
!
      end subroutine copy_rtp_zonal_grp_type_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_radial_grp_type_from_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      sph_grps%radial_rj_grp%num_grp =  num_radial_grp_rj_IO
      call allocate_grp_type_num(sph_grps%radial_rj_grp)
!
      if (sph_grps%radial_rj_grp%num_grp .gt. izero) then
!
        sph_grps%radial_rj_grp%istack_grp(0)                            &
     &        = istack_radial_grp_rj_IO(0)
        do i = 1, sph_grps%radial_rj_grp%num_grp
          sph_grps%radial_rj_grp%grp_name(i) = name_radial_grp_rj_IO(i)
          sph_grps%radial_rj_grp%istack_grp(i)                          &
     &        = istack_radial_grp_rj_IO(i)
        end do
!
        call allocate_grp_type_item(sph_grps%radial_rj_grp)
        do i = 1, sph_grps%radial_rj_grp%num_item
          sph_grps%radial_rj_grp%item_grp(i) = item_radial_grp_rj_IO(i)
        end do
      else
        sph_grps%radial_rj_grp%num_item = 0
        call allocate_grp_type_item(sph_grps%radial_rj_grp)
      end if
!
      call deallocate_rj_r_grp_IO_item
!
      end subroutine copy_rj_radial_grp_type_from_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_sphere_grp_type_from_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      sph_grps%sphere_rj_grp%num_grp =  num_sphere_grp_rj_IO
      call allocate_grp_type_num(sph_grps%sphere_rj_grp)
!
      if (sph_grps%sphere_rj_grp%num_grp .gt. izero) then
!
        sph_grps%sphere_rj_grp%istack_grp(0)                            &
     &        = istack_sphere_grp_rj_IO(0)
        do i = 1, sph_grps%sphere_rj_grp%num_grp
          sph_grps%sphere_rj_grp%grp_name(i) = name_sphere_grp_rj_IO(i)
          sph_grps%sphere_rj_grp%istack_grp(i)                          &
     &        = istack_sphere_grp_rj_IO(i)
        end do
!
        call allocate_grp_type_item(sph_grps%sphere_rj_grp)
        do i = 1, sph_grps%sphere_rj_grp%num_item
          sph_grps%sphere_rj_grp%item_grp(i) = item_sphere_grp_rj_IO(i)
        end do
      else
        sph_grps%sphere_rj_grp%num_item = 0
        call allocate_grp_type_item(sph_grps%sphere_rj_grp)
      end if
!
      call deallocate_rj_j_grp_IO_item
!
      end subroutine copy_rj_sphere_grp_type_from_IO
!
! -----------------------------------------------------------------------
!
      end module copy_sph_grps_type_from_IO
 