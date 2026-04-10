!>@file   set_sph_homogenious_BCs.f90
!!@brief  module set_sph_homogenious_BCs
!!
!!@author H. Matsui
!!@date    programmed by H.Matsui in Oct., 2009
!
!>@brief Set boundary conditions for scalar fields
!!
!!@verbatim
!!      subroutine find_both_sides_of_boundaries(sph_rj, radial_rj_grp, &
!!     &         bc_nod, bc_surf, sph_bc, igrp_icb, igrp_cmb)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(group_data), intent(in) :: radial_rj_grp
!!        type(boundary_condition_list), intent(in) :: bc_nod
!!        type(boundary_condition_list), intent(in) :: bc_surf
!!        type(sph_boundary_type), intent(inout) :: sph_bc
!!
!!      subroutine set_homogenious_scalar_bc(bc_name, bc_magnitude,     &
!!     &          sph_rj, ref_grp, bc_data, iflag_bc_scalar)
!!      subroutine set_homogenious_grad_bc(bc_name, bc_magnitude,       &
!!     &          sph_rj, ref_grp, iflag_bc_scalar, bc_data)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        character(len=kchara), intent(in) :: ref_grp
!!        character(len=kchara), intent(in) :: bc_name
!!        real(kind = kreal), intent(in) :: bc_magnitude
!!        real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
!!        integer(kind = kint), intent(inout) :: iflag_bc_scalar
!!@endverbatim
!
      module set_sph_homogenious_BCs
!
      use m_precision
      use calypso_mpi
!
      use m_constants
      use m_error_IDs
      use m_machine_parameter
      use m_boundary_condition_IDs
!
      use t_spheric_rj_data
      use t_group_data
      use t_boundary_params_sph_MHD
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine find_both_sides_of_boundaries(sph_rj, radial_rj_grp,   &
     &          bc_nod, bc_surf, sph_bc, igrp_icb, igrp_cmb)
!
      use t_bc_data_list
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(group_data), intent(in) :: radial_rj_grp
      type(boundary_condition_list), intent(in) :: bc_nod, bc_surf
!
      integer(kind = kint), intent(inout) :: igrp_icb, igrp_cmb
      type(sph_boundary_type), intent(inout) :: sph_bc
!
      integer(kind = kint) :: icou, i, j, inum, num, ibc_in, ibc_out
      integer(kind = kint) :: igrp_bc(2), kr_bc(2)
      character(len=kchara) :: r_bc_grp_name(2)
!
!
      icou = 0
      do i = 1, bc_nod%num_bc
        if(icou .ge. 2) exit
        do j = 1, radial_rj_grp%num_grp
          if(bc_nod%bc_name(i) .eq. radial_rj_grp%grp_name(j)) then
            num = radial_rj_grp%istack_grp(j)                           &
     &           - radial_rj_grp%istack_grp(j-1)
            if(num .ne. 1) go to 10
!
            icou = icou + 1
            inum = radial_rj_grp%istack_grp(j)
            kr_bc(icou) = radial_rj_grp%item_grp(inum)
            r_bc_grp_name(icou) = bc_nod%bc_name(i)
            igrp_bc(icou) = i
            exit
          end if
        end do
      end do
!
      do i = 1, bc_surf%num_bc
        if(icou .ge. 2) exit
        do j = 1, radial_rj_grp%num_grp
          if(bc_surf%bc_name(i) .eq. radial_rj_grp%grp_name(j)) then
            num = radial_rj_grp%istack_grp(j)                           &
     &           - radial_rj_grp%istack_grp(j-1)
            if(num .ne. 1) go to 10
!
            icou = icou + 1
            inum = radial_rj_grp%istack_grp(j)
            kr_bc(icou) = radial_rj_grp%item_grp(inum)
            r_bc_grp_name(icou) = bc_surf%bc_name(i)
            igrp_bc(icou) = -i
            exit
          end if
        end do
      end do
!
      if(kr_bc(1).le.0 .or. kr_bc(2).le.0 .or. kr_bc(1).eq.kr_bc(2))    &
     &     then
        write(*,*) 'Inner and outer boundary: ', kr_bc(1:2)
        go to 10
      else if(kr_bc(1) .gt. kr_bc(2)) then
        ibc_in =  2
        ibc_out = 1
      else
        ibc_in =  1
        ibc_out = 2
      end if
!
      igrp_icb = igrp_bc(ibc_in)
      sph_bc%kr_in =         kr_bc(ibc_in)
      sph_bc%icb_grp_name =  r_bc_grp_name(ibc_in)
!
      igrp_cmb = igrp_bc(ibc_out)
      sph_bc%kr_out =       kr_bc(ibc_out)
      sph_bc%cmb_grp_name = r_bc_grp_name(ibc_out)
!
      sph_bc%r_ICB(0) = sph_rj%radius_1d_rj_r(sph_bc%kr_in)
      sph_bc%r_ICB(1) = sph_rj%ar_1d_rj(sph_bc%kr_in,1)
      sph_bc%r_ICB(2) = sph_rj%ar_1d_rj(sph_bc%kr_in,2)
      sph_bc%r_CMB(0) = sph_rj%radius_1d_rj_r(sph_bc%kr_out)
      sph_bc%r_CMB(1) = sph_rj%ar_1d_rj(sph_bc%kr_out,1)
      sph_bc%r_CMB(2) = sph_rj%ar_1d_rj(sph_bc%kr_out,2)
!
      return
!
  10  continue
      call calypso_MPI_abort(ierr_BC, 'Set correct boundary condition')
!
      end subroutine find_both_sides_of_boundaries
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine set_homogenious_scalar_bc(bc_name, bc_magnitude,       &
     &          sph_rj, ref_grp, bc_data, iflag_bc_scalar)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      character(len=kchara), intent(in) :: ref_grp
      character(len=kchara), intent(in) :: bc_name
      real(kind = kreal), intent(in) :: bc_magnitude
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
!
      if(bc_name .ne. ref_grp) return
      iflag_bc_scalar =  iflag_fixed_field
!
      if(sph_rj%idx_rj_degree_zero .gt. 0)                              &
           bc_data(sph_rj%idx_rj_degree_zero) = bc_magnitude
!
      end subroutine set_homogenious_scalar_bc
!
! -----------------------------------------------------------------------
!
      subroutine set_homogenious_grad_bc(bc_name, bc_magnitude,         &
     &          sph_rj, ref_grp, iflag_bc_scalar, bc_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      character(len=kchara), intent(in) :: ref_grp
      character(len=kchara), intent(in) :: bc_name
      real(kind = kreal), intent(in) :: bc_magnitude
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
      integer(kind = kint), intent(inout) :: iflag_bc_scalar
!
!
      if(bc_name .ne. ref_grp) return
      iflag_bc_scalar =  iflag_fixed_flux
!
      if(sph_rj%idx_rj_degree_zero .gt. 0)                              &
     &      bc_data(sph_rj%idx_rj_degree_zero) = bc_magnitude
!
      end subroutine set_homogenious_grad_bc
!
! -----------------------------------------------------------------------
!
      end module set_sph_homogenious_BCs
