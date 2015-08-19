!>@file   coordinate_convert_4_sph.f90
!!@brief  module coordinate_convert_4_sph
!!
!!@author H. Matsui
!!@date Programmed in ????
!
!>@brief Get zonal mean and RMS fields in spherical grid
!!
!!@verbatim
!!      subroutine overwrite_nodal_sph_2_xyz
!!      subroutine overwrite_nodal_cyl_2_xyz
!!      subroutine overwrite_nodal_xyz_2_sph
!!      subroutine overwrite_nodal_sph_2_cyl
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
      use m_geometry_data
      use m_node_phys_data
!
      implicit  none
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_sph_2_xyz
!
      use cvt_sph_vector_2_xyz_smp
      use cvt_sph_tensor_2_xyz_smp
!
      integer(kind = kint) :: i, i_fld, numdir
!
!
!$omp parallel private(i,i_fld,numdir)
      do i = 1, num_nod_phys
        i_fld =  istack_nod_component(i-1) + 1
        numdir = istack_nod_component(i  ) - istack_nod_component(i-1)
        if     (numdir .eq. 6) then
          call overwrite_xyz_tensor_by_sph_smp                          &
     &       (np_smp, node1%numnod, node1%istack_nod_smp,               &
     &        d_nod(1,i_fld), node1%xx(1:node1%numnod,1),               &
     &        node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),   &
     &        node1%rr, node1%ss, node1%a_r, node1%a_s)
        else if(numdir .eq. 3) then
          call overwrite_sph_vect_2_xyz_smp                             &
     &       (np_smp, node1%numnod, node1%istack_nod_smp,               &
     &        d_nod(1,i_fld), node1%theta, node1%phi)
        end if
      end do
!$omp end parallel
!
      end subroutine overwrite_nodal_sph_2_xyz
!
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_cyl_2_xyz
!
      use cvt_cyl_vector_2_xyz_smp
      use cvt_cyl_tensor_2_xyz_smp
!
      integer(kind = kint) :: i, i_fld, numdir
!
!
!$omp parallel private(i,i_fld,numdir)
      do i = 1, num_nod_phys
        i_fld =  istack_nod_component(i-1) + 1
        numdir = istack_nod_component(i  ) - istack_nod_component(i-1)
        if     (numdir .eq. 6) then
          call overwrite_xyz_tensor_by_cyl_smp                          &
     &       (np_smp, node1%numnod, node1%istack_nod_smp,               &
     &        d_nod(1,i_fld),                                           &
     &        node1%xx(1:node1%numnod,1), node1%xx(1:node1%numnod,2),   &
     &        node1%ss, node1%a_s)
        else if(numdir .eq. 3) then
          call overwrite_cyl_vect_2_xyz_smp                             &
     &       (np_smp, node1%numnod, node1%istack_nod_smp,               &
     &        d_nod(1,i_fld), node1%phi)
        end if
      end do
!$omp end parallel
!
      end subroutine overwrite_nodal_cyl_2_xyz
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_xyz_2_sph
!
      use cvt_xyz_vector_2_sph_smp
      use cvt_xyz_tensor_2_sph_smp
!
      integer(kind = kint) :: i, i_fld, numdir
!
!
!$omp parallel private(i,i_fld,numdir)
      do i = 1, num_nod_phys
        i_fld =  istack_nod_component(i-1) + 1
        numdir = istack_nod_component(i  ) - istack_nod_component(i-1)
        if     (numdir .eq. 6) then
          call overwrite_sph_tensor_smp                                 &
     &       (np_smp, node1%numnod, node1%istack_nod_smp,               &
     &        d_nod(1,i_fld), node1%xx(1:node1%numnod,1),               &
     &        node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),   &
     &        node1%rr, node1%ss, node1%a_r, node1%a_s)
        else if(numdir .eq. 3) then
          call overwrite_vector_2_sph_smp                               &
     &       (np_smp, node1%numnod, node1%istack_nod_smp,               &
     &        d_nod(1,i_fld), node1%xx(1:node1%numnod,1),               &
     &        node1%xx(1:node1%numnod,2), node1%xx(1:node1%numnod,3),   &
     &        node1%rr, node1%ss, node1%a_r, node1%a_s)
        end if
      end do
!$omp end parallel
!
      end subroutine overwrite_nodal_xyz_2_sph
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine overwrite_nodal_sph_2_cyl
!
      use cvt_sph_vector_2_cyl_smp
      use cvt_sph_tensor_2_cyl_smp
!
      integer(kind = kint) :: i, i_fld, numdir
!
!
!$omp parallel private(i,i_fld,numdir)
      do i = 1, num_nod_phys
        i_fld =  istack_nod_component(i-1) + 1
        numdir = istack_nod_component(i  ) - istack_nod_component(i-1)
        if     (numdir .eq. 6) then
          call overwrite_cyl_tensor_by_sph_smp(np_smp, node1%numnod,    &
     &        node1%istack_nod_smp, d_nod(1,i_fld), node1%theta)
        else if(numdir .eq. 3) then
          call overwrite_sph_vect_2_cyl_smp(np_smp, node1%numnod,       &
     &        node1%istack_nod_smp, d_nod(1,i_fld), node1%theta)
        end if
      end do
!$omp end parallel
!
      end subroutine overwrite_nodal_sph_2_cyl
!
! -------------------------------------------------------------------
!
      end module coordinate_convert_4_sph
