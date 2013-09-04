!copy_sph_grps_type_to_IO.f90
!      module copy_sph_grps_type_to_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine copy_rtp_nod_grp_type_to_IO(sph_grps)
!      subroutine copy_rtp_radial_grp_type_to_IO(sph_grps)
!      subroutine copy_rtp_theta_grp_type_to_IO(sph_grps)
!      subroutine copy_rtp_zonal_grp_type_to_IO(sph_grps)
!      subroutine copy_rj_radial_grp_type_to_IO(sph_grps)
!      subroutine copy_rj_sphere_grp_type_to_IO(sph_grps)
!        type(sph_group_data), intent(inout) ::  sph_grps
!
      module copy_sph_grps_type_to_IO
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
      subroutine copy_rtp_nod_grp_type_to_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      num_bc_grp_rtp_IO =  sph_grps%bc_rtp_grp%num_grp
      call allocate_rtp_nod_grp_IO_stack
!
      if (sph_grps%bc_rtp_grp%num_grp .gt. izero) then
!
        ntot_bc_grp_rtp_IO = sph_grps%bc_rtp_grp%num_item
        call allocate_rtp_nod_grp_IO_item
!
        istack_bc_grp_rtp_IO(0) = sph_grps%bc_rtp_grp%istack_grp(0)
        do i = 1, sph_grps%bc_rtp_grp%num_grp
          name_bc_grp_rtp_IO(i) =   sph_grps%bc_rtp_grp%grp_name(i)
          istack_bc_grp_rtp_IO(i) = sph_grps%bc_rtp_grp%istack_grp(i)
        end do
        do i = 1, sph_grps%bc_rtp_grp%num_item
          item_bc_grp_rtp_IO(i) = sph_grps%bc_rtp_grp%item_grp(i)
        end do
      else
        ntot_bc_grp_rtp_IO = 0
        call allocate_rtp_nod_grp_IO_item
      end if
!
      call deallocate_grp_type_num(sph_grps%bc_rtp_grp)
!
      end subroutine copy_rtp_nod_grp_type_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_radial_grp_type_to_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      num_radial_grp_rtp_IO =  sph_grps%radial_rtp_grp%num_grp
      call allocate_rtp_r_grp_IO_stack
!
      if (sph_grps%radial_rtp_grp%num_grp .gt. izero) then
!
        ntot_radial_grp_rtp_IO = sph_grps%radial_rtp_grp%num_item
        call allocate_rtp_r_grp_IO_item
!
        istack_radial_grp_rtp_IO(0)                                     &
     &        = sph_grps%radial_rtp_grp%istack_grp(0)
        do i = 1, sph_grps%radial_rtp_grp%num_grp
          name_radial_grp_rtp_IO(i)                                     &
     &        = sph_grps%radial_rtp_grp%grp_name(i)
          istack_radial_grp_rtp_IO(i)                                   &
     &        = sph_grps%radial_rtp_grp%istack_grp(i)
        end do
        do i = 1, sph_grps%radial_rtp_grp%num_item
          item_radial_grp_rtp_IO(i)                   &
     &        = sph_grps%radial_rtp_grp%item_grp(i)
        end do
      else
        ntot_radial_grp_rtp_IO = 0
        call allocate_rtp_r_grp_IO_item
      end if
!
      call deallocate_grp_type_num(sph_grps%radial_rtp_grp)
!
      end subroutine copy_rtp_radial_grp_type_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_theta_grp_type_to_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      num_theta_grp_rtp_IO =  sph_grps%theta_rtp_grp%num_grp
      call allocate_rtp_t_grp_IO_stack
!
      if (sph_grps%theta_rtp_grp%num_grp .gt. izero) then
!
        ntot_theta_grp_rtp_IO = sph_grps%theta_rtp_grp%num_item
        call allocate_rtp_t_grp_IO_item
!
        istack_theta_grp_rtp_IO(0)                                      &
     &        = sph_grps%theta_rtp_grp%istack_grp(0)
        do i = 1, sph_grps%theta_rtp_grp%num_grp
          name_theta_grp_rtp_IO(i) = sph_grps%theta_rtp_grp%grp_name(i)
          istack_theta_grp_rtp_IO(i)                                    &
     &        = sph_grps%theta_rtp_grp%istack_grp(i)
        end do
        do i = 1, sph_grps%theta_rtp_grp%num_item
          item_theta_grp_rtp_IO(i) = sph_grps%theta_rtp_grp%item_grp(i)
        end do
      else
        ntot_theta_grp_rtp_IO = 0
        call allocate_rtp_t_grp_IO_item
      end if
!
      call deallocate_grp_type_num(sph_grps%theta_rtp_grp)
!
      end subroutine copy_rtp_theta_grp_type_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rtp_zonal_grp_type_to_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      num_zonal_grp_rtp_IO =  sph_grps%zonal_rtp_grp%num_grp
      call allocate_rtp_p_grp_IO_stack
!
      if (sph_grps%zonal_rtp_grp%num_grp .gt. izero) then
!
        ntot_zonal_grp_rtp_IO = sph_grps%zonal_rtp_grp%num_item
        call allocate_rtp_p_grp_IO_item
!
        istack_zonal_grp_rtp_IO(0)                                      &
     &        = sph_grps%zonal_rtp_grp%istack_grp(0)
        do i = 1, sph_grps%zonal_rtp_grp%num_grp
          name_zonal_grp_rtp_IO(i) = sph_grps%zonal_rtp_grp%grp_name(i)
          istack_zonal_grp_rtp_IO(i)                                    &
     &        = sph_grps%zonal_rtp_grp%istack_grp(i)
        end do
        do i = 1, sph_grps%zonal_rtp_grp%num_item
          item_zonal_grp_rtp_IO(i) = sph_grps%zonal_rtp_grp%item_grp(i)
        end do
      else
        ntot_zonal_grp_rtp_IO = 0
        call allocate_rtp_p_grp_IO_item
      end if
!
      call deallocate_grp_type_num(sph_grps%zonal_rtp_grp)
!
      end subroutine copy_rtp_zonal_grp_type_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_radial_grp_type_to_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      num_radial_grp_rj_IO =  sph_grps%radial_rj_grp%num_grp
      call allocate_rj_r_grp_IO_stack
!
      if (sph_grps%radial_rj_grp%num_grp .gt. izero) then
!
        ntot_radial_grp_rj_IO = sph_grps%radial_rj_grp%num_item
        call allocate_rj_r_grp_IO_item
!
        istack_radial_grp_rj_IO(0)                                      &
     &        = sph_grps%radial_rj_grp%istack_grp(0)
        do i = 1, sph_grps%radial_rj_grp%num_grp
          name_radial_grp_rj_IO(i) = sph_grps%radial_rj_grp%grp_name(i)
          istack_radial_grp_rj_IO(i)                                    &
     &        = sph_grps%radial_rj_grp%istack_grp(i)
        end do
        do i = 1, sph_grps%radial_rj_grp%num_item
          item_radial_grp_rj_IO(i) = sph_grps%radial_rj_grp%item_grp(i)
        end do
      else
        ntot_radial_grp_rj_IO = 0
        call allocate_rj_r_grp_IO_item
      end if
!
      call deallocate_grp_type_num(sph_grps%radial_rj_grp)
!
      end subroutine copy_rj_radial_grp_type_to_IO
!
! -----------------------------------------------------------------------
!
      subroutine copy_rj_sphere_grp_type_to_IO(sph_grps)
!
      type(sph_group_data), intent(inout) ::  sph_grps
      integer(kind = kint) :: i
!
!
      num_sphere_grp_rj_IO =  sph_grps%sphere_rj_grp%num_grp
      call allocate_rj_j_grp_IO_stack
!
      if (sph_grps%sphere_rj_grp%num_grp .gt. izero) then
!
        ntot_sphere_grp_rj_IO = sph_grps%sphere_rj_grp%num_item
        call allocate_rj_j_grp_IO_item
!
        istack_sphere_grp_rj_IO(0)                                      &
     &        = sph_grps%sphere_rj_grp%istack_grp(0)
        do i = 1, sph_grps%sphere_rj_grp%num_grp
          name_sphere_grp_rj_IO(i) = sph_grps%sphere_rj_grp%grp_name(i)
          istack_sphere_grp_rj_IO(i)                                    &
     &        = sph_grps%sphere_rj_grp%istack_grp(i)
        end do
        do i = 1, sph_grps%sphere_rj_grp%num_item
          item_sphere_grp_rj_IO(i) = sph_grps%sphere_rj_grp%item_grp(i)
        end do
      else
        sph_grps%sphere_rj_grp%num_item = 0
        call allocate_rj_j_grp_IO_item
      end if
!
      call deallocate_grp_type_num(sph_grps%sphere_rj_grp)
!
      end subroutine copy_rj_sphere_grp_type_to_IO
!
! -----------------------------------------------------------------------
!
      end module copy_sph_grps_type_to_IO
 