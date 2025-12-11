!> @file  set_each_sph_bc_IO_data.f90
!!      module set_each_sph_bc_IO_data
!!
!! @author  H. Matsui
!! @date Programmed in Dec. 2012
!
!> @brief Boundary condition data from external file
!!
!!@verbatim
!!      subroutine set_bc_4_sph_scalar_by_file(sph_rj, bc_ctls, bc_data)
!!      subroutine set_bc_4_sph_vector_by_file                          &
!!     &         (sph_rj, bc_ctls, vp_data, dp_data, vt_data)
!!      subroutine bc_4_evo_scalar_sph_by_file(sph_rj, bc_ctls, bc_data)
!!      subroutine bc_4_evo_vect2_sph_by_file                           &
!!     &         (sph_rj, bc_ctls, vp_data, vt_data)
!!      subroutine bc_4_evo_vector_sph_by_file(sph_rj, bc_ctls,         &
!!     &          vp_data, dp_data, vt_data)
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(each_boundary_spectr), intent(in) :: bc_ctls
!!  ---------------------------------------------------------------------
!!       line 2:     Field name to define the first boundary condition
!!       line 3:     Place to define the first boundary condition
!!       line 4:     Number of spherical harmonics modes 
!!                    for each boundary condition
!!       line 5...:  Spectrum data for the boundary conditions 
!!                  (degree $l$, order $m$, and harmonics coefficients)
!!        Return to 2...
!!  ---------------------------------------------------------------------
!!@endverbatim
!
      module set_each_sph_bc_IO_data
!
      use m_precision
      use t_spheric_rj_data
      use t_each_sph_boundary_IO_data
!
      implicit  none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_sph_scalar_by_file(sph_rj, bc_ctls, bc_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) bc_data(j) =  bc_ctls%bc_input(inum,1)
      end do
!
      end subroutine set_bc_4_sph_scalar_by_file
!
! -----------------------------------------------------------------------
!
      subroutine set_bc_4_sph_vector_by_file                            &
     &         (sph_rj, bc_ctls, vp_data, dp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: dp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          dp_data(j) = bc_ctls%bc_input(inum,2)
          vt_data(j) = bc_ctls%bc_input(inum,3)
        end if
      end do
!
      end subroutine set_bc_4_sph_vector_by_file
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine bc_4_evo_scalar_sph_by_file(sph_rj, bc_ctls, bc_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: bc_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) bc_data(j) = bc_ctls%bc_input(inum,1)
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) bc_data(j) = bc_ctls%bc_input(inum,1)
      end do
!
      end subroutine bc_4_evo_scalar_sph_by_file
!
! -----------------------------------------------------------------------
!
      subroutine bc_4_evo_vect2_sph_by_file                             &
     &         (sph_rj, bc_ctls, vp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          vt_data(j) = bc_ctls%bc_input(inum,2)
        end if
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          vt_data(j) = bc_ctls%bc_input(inum,2)
        end if
      end do
!
      end subroutine bc_4_evo_vect2_sph_by_file
!
! -----------------------------------------------------------------------
!
      subroutine bc_4_evo_vector_sph_by_file(sph_rj, bc_ctls,           &
     &          vp_data, dp_data, vt_data)
!
      type(sph_rj_grid), intent(in) :: sph_rj
      type(each_boundary_spectr), intent(in) :: bc_ctls
!
      real(kind = kreal), intent(inout) :: vp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: dp_data(sph_rj%nidx_rj(2))
      real(kind = kreal), intent(inout) :: vt_data(sph_rj%nidx_rj(2))
!
      integer(kind = kint) :: inum, j
      integer :: l, m
!
!
      do inum = 1, bc_ctls%num_bc_mode
        l = int(bc_ctls%imode_gl(1,inum))
        m = int(bc_ctls%imode_gl(2,inum))
        j = find_local_sph_address(sph_rj, l, m)
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          dp_data(j) = bc_ctls%bc_input(inum,2)
          vt_data(j) = bc_ctls%bc_input(inum,3)
        end if
!
        if(m .eq. 0) cycle
        j = find_local_sph_address(sph_rj, l, (-m))
        if(j .gt. 0) then
          vp_data(j) = bc_ctls%bc_input(inum,1)
          dp_data(j) = bc_ctls%bc_input(inum,2)
          vt_data(j) = bc_ctls%bc_input(inum,3)
        end if
      end do
!
      end subroutine bc_4_evo_vector_sph_by_file
!
! -----------------------------------------------------------------------
!
      end module set_each_sph_bc_IO_data
