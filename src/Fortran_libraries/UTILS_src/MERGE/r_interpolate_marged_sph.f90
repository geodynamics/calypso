!>@file   r_interpolate_marged_sph.f90
!!@brief  module r_interpolate_marged_sph
!!
!!@author H. Matsui
!!@date Programmed in Jan., 2014
!
!>@brief Radial interpolation for assemble program
!!
!!@verbatim
!!      subroutine const_r_interpolate_table                            &
!!     &         (org_sph_mesh, new_sph_mesh, r_itp)
!!      subroutine const_ICB_and_CMB_radius(org_sph_mesh, new_sph_mesh, &
!!     &          nlayer_ICB_org, nlayer_CMB_org,                       &
!!     &          nlayer_ICB_new, nlayer_CMB_new)
!!      subroutine deallocate_radial_itp_tbl(nri_new)
!!      subroutine sph_radial_interpolation_coef                        &
!!     &         (nri_org, r_org, nri_new, r_new)
!!@endverbatim
!!
!!@param   nnod_org  Number of spectr data for original data
!!@param   nri_org   Number of radial grid for original data
!!@param   jmax_org  Number of harmonics modes for original data
!!@param   r_org     Position of radial grid for original data
!!@param   idx_gl_1d_j_org(jmax_org,3)
!!                  List of spherical harmonics modes for original data
!!@param   d_rj_IO   Read harmonics data data
!
      module r_interpolate_marged_sph
!
      use m_precision
      use m_constants
      use calypso_mpi
      use t_SPH_mesh_field_data
!
      implicit none
!
      type sph_radial_itp_data
!>      Integer flag if radial grid is same
        integer(kind = kint) :: iflag_same_rgrid =  1
!
!>      Number of radial grids for new spectr data
        integer(kind = kint) :: nri_old2new =  0
!>      Inner radial grid ID for interpolation
        integer(kind = kint), allocatable :: k_old2new_in(:)
!>      Outer radial grid ID for interpolation
        integer(kind = kint), allocatable :: k_old2new_out(:)
!>      Coefficient for Inner grid data for interpolation
        real(kind = kreal), allocatable :: coef_old2new_in(:)
!
!>      Innermost new radial ID within the original domain
        integer(kind = kint) :: kr_inner_domain =  0
!>      Outmost new radial ID within the original domain
        integer(kind = kint) :: kr_outer_domain = 0
      end type sph_radial_itp_data
!
      private :: sph_radial_interpolation_coef
      private :: share_r_interpolation_tbl
      private :: set_sph_boundary_4_merge
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine const_r_interpolate_table                              &
     &         (org_sph_mesh, new_sph_mesh, r_itp)
!
      type(sph_mesh_data), intent(in) :: org_sph_mesh
      type(sph_mesh_data), intent(in) :: new_sph_mesh
!
      type(sph_radial_itp_data), intent(inout) :: r_itp
!
!
      if(my_rank .eq. 0) then
        call sph_radial_interpolation_coef                              &
     &     (org_sph_mesh%sph%sph_rj%nidx_rj(1),                         &
     &      org_sph_mesh%sph%sph_rj%radius_1d_rj_r,                     &
     &      new_sph_mesh%sph%sph_rj%nidx_rj(1),                         &
     &      new_sph_mesh%sph%sph_rj%radius_1d_rj_r, r_itp)
      end if
      call share_r_interpolation_tbl(new_sph_mesh, r_itp)
!
      end subroutine const_r_interpolate_table
!
! -----------------------------------------------------------------------
!
      subroutine const_ICB_and_CMB_radius(org_sph_mesh, new_sph_mesh,   &
     &          nlayer_ICB_org, nlayer_CMB_org,                         &
     &          nlayer_ICB_new, nlayer_CMB_new)
!
      type(sph_mesh_data), intent(in) :: org_sph_mesh
      type(sph_mesh_data), intent(in) :: new_sph_mesh
!
      integer(kind = kint), intent(inout) :: nlayer_ICB_org
      integer(kind = kint), intent(inout) :: nlayer_CMB_org
      integer(kind = kint), intent(inout) :: nlayer_ICB_new
      integer(kind = kint), intent(inout) :: nlayer_CMB_new
!
!
      if(my_rank .eq. 0) then
        call set_sph_boundary_4_merge(org_sph_mesh%sph_grps,            &
     &      nlayer_ICB_org, nlayer_CMB_org)
        call set_sph_boundary_4_merge(new_sph_mesh%sph_grps,            &
     &      nlayer_ICB_new, nlayer_CMB_new)
      end if
!
      call share_ICB_and_CMB_radius(nlayer_ICB_org, nlayer_CMB_org,     &
     &   nlayer_ICB_new, nlayer_CMB_new)
!
      end subroutine const_ICB_and_CMB_radius
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_radial_itp_tbl(r_itp)
!
      type(sph_radial_itp_data), intent(inout) :: r_itp
!
!
      deallocate(r_itp%k_old2new_in, r_itp%k_old2new_out)
      deallocate(r_itp%coef_old2new_in)
!
      end subroutine deallocate_radial_itp_tbl
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine allocate_radial_itp_tbl(nri_new, r_itp)
!
      integer(kind = kint), intent(in) :: nri_new
      type(sph_radial_itp_data), intent(inout) :: r_itp
!
!
      r_itp%nri_old2new = nri_new
      allocate(r_itp%k_old2new_in(r_itp%nri_old2new))
      allocate(r_itp%k_old2new_out(r_itp%nri_old2new))
      allocate(r_itp%coef_old2new_in(r_itp%nri_old2new))
!
      r_itp%k_old2new_in =    0
      r_itp%k_old2new_out =   0
      r_itp%coef_old2new_in = 0.0d0
!
      end subroutine allocate_radial_itp_tbl
!
! -----------------------------------------------------------------------
!
      subroutine set_sph_boundary_4_merge(sph_grps,                     &
     &          nlayer_ICB, nlayer_CMB)
!
      use t_spheric_parameter
      use skip_comment_f
!
      type(sph_group_data), intent(in) ::  sph_grps
      integer(kind = kint), intent(inout) :: nlayer_ICB, nlayer_CMB
!
      integer(kind = kint) :: k, kk
      character(len = kchara) :: tmpchara
!
!
      do k = 1, sph_grps%radial_rj_grp%num_grp
        tmpchara = sph_grps%radial_rj_grp%grp_name(k)
        if(cmp_no_case(tmpchara, ICB_nod_grp_name)) then
          kk = sph_grps%radial_rj_grp%istack_grp(k-1) + 1
          nlayer_ICB = sph_grps%radial_rj_grp%item_grp(kk)
        else if(cmp_no_case(tmpchara, CMB_nod_grp_name)) then
          kk = sph_grps%radial_rj_grp%istack_grp(k-1) + 1
          nlayer_CMB = sph_grps%radial_rj_grp%item_grp(kk)
        end if
      end do
!
      end subroutine set_sph_boundary_4_merge
!
! -----------------------------------------------------------------------
!
      subroutine share_r_interpolation_tbl(new_sph_mesh, r_itp)
!
      type(sph_mesh_data), intent(in) :: new_sph_mesh
      type(sph_radial_itp_data), intent(inout) :: r_itp
!
!
      call MPI_Bcast(r_itp%iflag_same_rgrid,                            &
     &    1, CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(new_sph_mesh%sph%sph_rj%nidx_rj(1),                &
     &    1, CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
      if(my_rank .eq. 0) write(*,*) 'iflag_same_rgrid: ',               &
     &            r_itp%iflag_same_rgrid,                               &
     &            new_sph_mesh%sph%sph_rj%nidx_rj(1)
!
      if(r_itp%iflag_same_rgrid .eq. 0) then
        if(my_rank .ne. 0)  call allocate_radial_itp_tbl                &
     &             (new_sph_mesh%sph%sph_rj%nidx_rj(1), r_itp)
!
        call MPI_Bcast(r_itp%nri_old2new, 1, CALYPSO_INTEGER,           &
     &      0, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%kr_inner_domain, 1, CALYPSO_INTEGER,       &
     &      0, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%kr_outer_domain, 1, CALYPSO_INTEGER,       &
     &      0, CALYPSO_COMM, ierr_MPI)
!
        call MPI_Bcast(r_itp%k_old2new_in, int(r_itp%nri_old2new),      &
     &      CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%k_old2new_out, int(r_itp%nri_old2new),     &
     &      CALYPSO_INTEGER, 0, CALYPSO_COMM, ierr_MPI)
        call MPI_Bcast(r_itp%coef_old2new_in, int(r_itp%nri_old2new),   &
     &      CALYPSO_REAL, 0, CALYPSO_COMM, ierr_MPI)
      end if
!
      end subroutine share_r_interpolation_tbl
!
! -----------------------------------------------------------------------
!
      subroutine share_ICB_and_CMB_radius                               &
     &         (nlayer_ICB_org, nlayer_CMB_org,                         &
     &          nlayer_ICB_new, nlayer_CMB_new)
!
      integer(kind = kint), intent(inout) :: nlayer_ICB_org
      integer(kind = kint), intent(inout) :: nlayer_CMB_org
      integer(kind = kint), intent(inout) :: nlayer_ICB_new
      integer(kind = kint), intent(inout) :: nlayer_CMB_new
!
!
      call MPI_Bcast(nlayer_ICB_org, 1, CALYPSO_INTEGER, 0,             &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nlayer_CMB_org, 1, CALYPSO_INTEGER, 0,             &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nlayer_ICB_new, 1, CALYPSO_INTEGER, 0,             &
     &    CALYPSO_COMM, ierr_MPI)
      call MPI_Bcast(nlayer_CMB_new, 1, CALYPSO_INTEGER, 0,             &
     &    CALYPSO_COMM, ierr_MPI)
!
      if(my_rank .eq. 0) then
        write(*,*) 'nlayer_ICB_org: ', nlayer_ICB_org, nlayer_CMB_org
        write(*,*) 'nlayer_ICB_new: ', nlayer_ICB_new, nlayer_CMB_new
      end if
!
      end subroutine share_ICB_and_CMB_radius
!
! -----------------------------------------------------------------------
!
      subroutine sph_radial_interpolation_coef                          &
     &         (nri_org, r_org, nri_new, r_new, r_itp)
!
      integer(kind = kint), intent(in) :: nri_org, nri_new
      real(kind = kreal), intent(in) :: r_org(nri_org)
      real(kind = kreal), intent(in) :: r_new(nri_org)
      type(sph_radial_itp_data), intent(inout) :: r_itp
!
      integer(kind = kint) :: k, kr_org
      real(kind = kreal) :: r_in, r_out
!
!
      r_itp%iflag_same_rgrid = 1
      if(nri_org .ne. nri_new) then
        r_itp%iflag_same_rgrid =  0
      else
        do k = 1, nri_new
          if(abs(r_new(k) - r_org(k)) .gt. 1.0E-12) then
            r_itp%iflag_same_rgrid = 0
            exit
          end if
        end do
      end if
!
!      write(*,*) 'r_itp%iflag_same_rgrid', r_itp%iflag_same_rgrid
      if(r_itp%iflag_same_rgrid .ne. 0) return
!
      call allocate_radial_itp_tbl(nri_new, r_itp)
!
      do k = 1, nri_new
        if(r_new(k) .lt. r_org(1)) then
          r_itp%k_old2new_in(k) =    0
          r_itp%k_old2new_out(k) =   1
          r_itp%coef_old2new_in(k) = -1.0d0
        else if(r_new(k) .eq. r_org(1)) then
          r_itp%k_old2new_in(k) =    1
          r_itp%k_old2new_out(k) =   2
          r_itp%coef_old2new_in(k) = 1.0d0
        else if(r_new(k) .eq. r_org(nri_org)) then
          r_itp%k_old2new_in(k) =    nri_org - 1
          r_itp%k_old2new_out(k) =   nri_org
          r_itp%coef_old2new_in(k) = 0.0d0
        else if(r_new(k) .gt. r_org(nri_org)) then
          r_itp%k_old2new_in(k) =    nri_org
          r_itp%k_old2new_out(k) =   nri_org + 1
          r_itp%coef_old2new_in(k) = -1.0d0
        else
          do kr_org = 1, nri_org
            r_in =  r_org(kr_org-1)
            r_out = r_org(kr_org  )
            if(r_new(k) .ge. r_in  .and. r_new(k) .lt. r_out) then
              r_itp%k_old2new_in(k) =  kr_org - 1
              r_itp%k_old2new_out(k) = kr_org
              r_itp%coef_old2new_in(k)                                  &
     &                         = (r_out - r_new(k)) / (r_out - r_in)
              exit
            end if
          end do
        end if
      end do
!
      r_itp%kr_inner_domain = 1
      do k = nri_new, 1, -1
        if(r_new(k) .lt. r_org(1)) then
          r_itp%kr_inner_domain = k + 1
          exit
        end if
      end do
      r_itp%kr_outer_domain = nri_new
      do k = 1, nri_new
        if(r_new(k) .gt. r_org(nri_org)) then
          r_itp%kr_outer_domain = k - 1
          exit
        end if
      end do
!
      write(*,*) 'r_itp%kr_inner_domain', r_itp%kr_inner_domain
      write(*,*) 'r_itp%kr_outer_domain', r_itp%kr_outer_domain
      do k = 1, nri_new
        write(*,'(i5,1pe16.8,2i5,1p3e16.8)') k, r_new(k),             &
     &         r_itp%k_old2new_in(k), r_itp%k_old2new_out(k),         &
     &         r_org(r_itp%k_old2new_in(k)),                          &
     &         r_org(r_itp%k_old2new_out(k)),                         &
     &         r_itp%coef_old2new_in(k)
      end do
!
      end subroutine sph_radial_interpolation_coef
!
! -----------------------------------------------------------------------
!
      end module r_interpolate_marged_sph
