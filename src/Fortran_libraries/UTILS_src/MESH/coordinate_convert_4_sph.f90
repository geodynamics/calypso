!>@file   coordinate_convert_4_sph.f90
!!@brief  module coordinate_convert_4_sph
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Get zonal mean and RMS fields in spherical grid
!!
!!@verbatim
!!      subroutine overwrite_nodal_sph_2_xyz(node, nod_fld)
!!      subroutine overwrite_nodal_cyl_2_xyz(node, nod_fld)
!!      subroutine overwrite_nodal_xyz_2_sph(node, nod_fld)
!!      subroutine overwrite_nodal_sph_2_cyl(node, nod_fld)
!!        type(node_data), intent(in) ::    node
!!        type(phys_data), intent(inout) :: nod_fld
!!@endverbatim
!!
!!@n @param  numdir     Number of component of field
!!@n @param  irtp_fld   Start address for field @f$ f(\r,\theta\phi) @f$
!
      module coordinate_convert_4_sph
!
      use m_precision
      use m_constants
      use m_machine_parameter
!
      use t_geometry_data
!
      implicit  none
!
      private :: overwrite_nodal_cyl_2_xyz_smp
      private :: overwrite_nodal_sph_2_cyl_smp
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_sph_2_xyz(node, nod_fld)
!
      use t_geometry_data
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, i_fld, numdir
!
!
!$omp parallel private(i,i_fld,numdir)
      do i = 1, nod_fld%num_phys
        i_fld =  nod_fld%istack_component(i-1) + 1
        numdir = nod_fld%num_component(i)
        call overwrite_nodal_sph_2_xyz_smp                              &
     &     (node, nod_fld%ntot_phys, i_fld, numdir, nod_fld%d_fld)
      end do
!$omp end parallel
!
      end subroutine overwrite_nodal_sph_2_xyz
!
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_cyl_2_xyz(node, nod_fld)
!
      use t_geometry_data
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, i_fld, numdir
!
!
!$omp parallel private(i,i_fld,numdir)
      do i = 1, nod_fld%num_phys
        i_fld =  nod_fld%istack_component(i-1) + 1
        numdir = nod_fld%num_component(i)
        call overwrite_nodal_cyl_2_xyz_smp                              &
     &     (node, nod_fld%ntot_phys, i_fld, numdir, nod_fld%d_fld)
      end do
!$omp end parallel
!
      end subroutine overwrite_nodal_cyl_2_xyz
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_xyz_2_sph(node, nod_fld)
!
      use t_geometry_data
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, i_fld, numdir
!
!
!$omp parallel private(i,i_fld,numdir)
      do i = 1, nod_fld%num_phys
        i_fld =  nod_fld%istack_component(i-1) + 1
        numdir = nod_fld%num_component(i)
        call overwrite_nodal_xyz_2_sph_smp                              &
     &     (node, nod_fld%ntot_phys, i_fld, numdir, nod_fld%d_fld)
      end do
!$omp end parallel
!
      end subroutine overwrite_nodal_xyz_2_sph
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_sph_2_cyl(node, nod_fld)
!
      use t_geometry_data
      use t_phys_data
!
      type(node_data), intent(in) ::    node
      type(phys_data), intent(inout) :: nod_fld
!
      integer(kind = kint) :: i, i_fld, numdir
!
!
!$omp parallel private(i,i_fld,numdir)
      do i = 1, nod_fld%num_phys
        i_fld =  nod_fld%istack_component(i-1) + 1
        numdir = nod_fld%num_component(i)
        call overwrite_nodal_sph_2_cyl_smp                              &
     &     (node, nod_fld%ntot_phys, i_fld, numdir, nod_fld%d_fld)
      end do
!$omp end parallel
!
      end subroutine overwrite_nodal_sph_2_cyl
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_sph_2_xyz_smp                          &
     &         (node, ntot_comp, i_fld, numdir, d_nod)
!
      use cvt_sph_vector_2_xyz_smp
      use cvt_sph_tensor_2_xyz_smp
!
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(in) :: ntot_comp, i_fld, numdir
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      if     (numdir .eq. 6) then
        call overwrite_xyz_tensor_by_sph_smp                            &
     &       (np_smp, node%numnod, node%istack_nod_smp,                 &
     &        d_nod(1,i_fld), node%xx(1:node%numnod,1),                 &
     &        node%xx(1:node%numnod,2), node%xx(1:node%numnod,3),       &
     &        node%rr, node%ss, node%a_r, node%a_s)
      else if(numdir .eq. 3) then
        call overwrite_sph_vect_2_xyz_smp                               &
     &       (np_smp, node%numnod, node%istack_nod_smp,                 &
     &        d_nod(1,i_fld), node%theta, node%phi)
      end if
!
      end subroutine overwrite_nodal_sph_2_xyz_smp
!
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_cyl_2_xyz_smp                          &
     &         (node, ntot_comp, i_fld, numdir, d_nod)
!
      use cvt_cyl_vector_2_xyz_smp
      use cvt_cyl_tensor_2_xyz_smp
!
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(in) :: ntot_comp, i_fld, numdir
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      if     (numdir .eq. 6) then
        call overwrite_xyz_tensor_by_cyl_smp                            &
     &       (np_smp, node%numnod, node%istack_nod_smp, d_nod(1,i_fld), &
     &        node%xx(1:node%numnod,1), node%xx(1:node%numnod,2),       &
     &        node%ss, node%a_s)
      else if(numdir .eq. 3) then
        call overwrite_cyl_vect_2_xyz_smp                               &
     &       (np_smp, node%numnod, node%istack_nod_smp,                 &
     &        d_nod(1,i_fld), node%phi)
      end if
!
      end subroutine overwrite_nodal_cyl_2_xyz_smp
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_xyz_2_sph_smp                          &
     &         (node, ntot_comp, i_fld, numdir, d_nod)
!
      use cvt_xyz_vector_2_sph_smp
      use cvt_xyz_tensor_2_sph_smp
!
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(in) :: ntot_comp, i_fld, numdir
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      if     (numdir .eq. 6) then
        call overwrite_sph_tensor_smp                                   &
     &       (np_smp, node%numnod, node%istack_nod_smp,                 &
     &        d_nod(1,i_fld), node%xx(1:node%numnod,1),                 &
     &        node%xx(1:node%numnod,2), node%xx(1:node%numnod,3),       &
     &        node%rr, node%ss, node%a_r, node%a_s)
      else if(numdir .eq. 3) then
        call overwrite_vector_2_sph_smp                                 &
     &       (np_smp, node%numnod, node%istack_nod_smp,                 &
     &        d_nod(1,i_fld), node%xx(1:node%numnod,1),                 &
     &        node%xx(1:node%numnod,2), node%xx(1:node%numnod,3),       &
     &        node%rr, node%ss, node%a_r, node%a_s)
      end if
!
      end subroutine overwrite_nodal_xyz_2_sph_smp 
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_sph_2_cyl_smp                          &
     &         (node, ntot_comp, i_fld, numdir, d_nod)
!
      use cvt_sph_vector_2_cyl_smp
      use cvt_sph_tensor_2_cyl_smp
!
      type(node_data), intent(in) :: node
!
      integer(kind = kint), intent(in) :: ntot_comp, i_fld, numdir
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_nod(node%numnod,ntot_comp)
!
!
      if     (numdir .eq. 6) then
        call overwrite_cyl_tensor_by_sph_smp(np_smp, node%numnod,       &
     &        node%istack_nod_smp, d_nod(1,i_fld), node%theta)
      else if(numdir .eq. 3) then
        call overwrite_sph_vect_2_cyl_smp(np_smp, node%numnod,          &
     &        node%istack_nod_smp, d_nod(1,i_fld), node%theta)
      end if
!
      end subroutine overwrite_nodal_sph_2_cyl_smp
!
! -------------------------------------------------------------------
!
      end module coordinate_convert_4_sph
