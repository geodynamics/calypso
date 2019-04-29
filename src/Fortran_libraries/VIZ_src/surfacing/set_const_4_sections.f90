!set_const_4_sections.f90
!      module set_const_4_sections
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine set_const_4_crossections                             &
!!     &         (num_psf, psf_def, node, psf_list)
!!        type(node_data), intent(in) :: node
!!        type(section_define), intent(in):: psf_def(num_psf)
!!        type(sectioning_list), intent(inout):: psf_list(num_psf)
!!      subroutine set_const_4_isosurfaces                              &
!!     &         (num_iso, node, nod_fld, iso_def, iso_list)
!!        type(node_data), intent(in) :: node
!!        type(phys_data), intent(in) :: nod_fld
!!        type(isosurface_define), intent(in) :: iso_def(num_iso)
!!        type(sectioning_list), intent(inout):: iso_list(num_iso)
!
      module set_const_4_sections
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      implicit none
!
      private :: set_constant_4_psf, set_constant_4_iso
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine set_const_4_crossections                               &
     &         (num_psf, psf_def, node, psf_list)
!
      use t_control_params_4_psf
      use t_geometry_data
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: num_psf
      type(node_data), intent(in) :: node
      type(section_define), intent(in):: psf_def(num_psf)
!
      type(sectioning_list), intent(inout):: psf_list(num_psf)
!
      integer(kind = kint) :: i_psf
!
!
      do i_psf = 1, num_psf
        if(psf_def(i_psf)%id_section_method .gt. 0) then
          call set_constant_4_psf                                       &
     &       (node%numnod, node%istack_nod_smp, node%xx,                &
     &        psf_def(i_psf)%const_psf, psf_list(i_psf)%ref_fld)
        end if
      end do
!
      end subroutine set_const_4_crossections
!
!  ---------------------------------------------------------------------
!
      subroutine set_const_4_isosurfaces                                &
     &         (num_iso, node, nod_fld, iso_def, iso_list)
!
      use t_control_params_4_iso
      use t_geometry_data
      use t_phys_data
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: num_iso
      type(node_data), intent(in) :: node
      type(phys_data), intent(in) :: nod_fld
      type(isosurface_define), intent(in) :: iso_def(num_iso)
!
      type(sectioning_list), intent(inout):: iso_list(num_iso)
!
      integer(kind = kint) :: i_iso
!
!
      do i_iso = 1, num_iso
        call set_constant_4_iso                                         &
     &     (iso_def(i_iso), node%numnod, node%istack_nod_smp, node%xx,  &
     &      node%rr, node%a_r, node%ss, node%a_s, nod_fld%num_phys,     &
     &      nod_fld%ntot_phys,  nod_fld%istack_component,               &
     &      nod_fld%d_fld, iso_list(i_iso)%ref_fld)
      end do
!
      end subroutine set_const_4_isosurfaces
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_constant_4_psf(nnod, istack_nod_smp, xx,           &
     &          const_psf, c_ref_psf)
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
      real(kind = kreal), intent(in) :: xx(nnod,3)
      real(kind = kreal), intent(in) :: const_psf(10)
!
      real(kind = kreal), intent(inout) :: c_ref_psf(nnod)
!
      integer(kind = kint) :: ip, inod, ist, ied
!
!
!$omp parallel do private(ip,inod,ist,ied)
      do ip = 1, np_smp
        ist = istack_nod_smp(ip-1) + 1
        ied = istack_nod_smp(ip)
        do inod = ist, ied
          c_ref_psf(inod)                                               &
     &          =  const_psf(1) * (xx(inod,1)*xx(inod,1))               &
     &           + const_psf(2) * (xx(inod,2)*xx(inod,2))               &
     &           + const_psf(3) * (xx(inod,3)*xx(inod,3))               &
     &           + const_psf(4) * (xx(inod,1)*xx(inod,2))               &
     &           + const_psf(5) * (xx(inod,2)*xx(inod,3))               &
     &           + const_psf(6) * (xx(inod,3)*xx(inod,1))               &
     &           + const_psf(7) *  xx(inod,1)                           &
     &           + const_psf(8) *  xx(inod,2)                           &
     &           + const_psf(9) *  xx(inod,3)                           &
     &           + const_psf(10)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_constant_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_constant_4_iso(iso_def, nnod, istack_nod_smp,      &
     &          xx, radius, a_r, s_radius, a_s, num_fld, ntot_comp,     &
     &          istack_comp_nod, d_nod, c_ref_iso)
!
      use t_control_params_4_iso
!
      use mag_of_field_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_xyz_vector_2_cyl_smp
      use cal_subtract_smp
!
      use copy_field_smp
!
      type(isosurface_define), intent(in) :: iso_def
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
      real(kind = kreal), intent(in) :: xx(nnod,3)
      real(kind = kreal), intent(in) :: radius(nnod)
      real(kind = kreal), intent(in) :: a_r(nnod)
      real(kind = kreal), intent(in) :: s_radius(nnod)
      real(kind = kreal), intent(in) :: a_s(nnod)
!
      integer(kind = kint), intent(in) :: num_fld, ntot_comp
      integer(kind = kint), intent(in) :: istack_comp_nod(0:num_fld)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      real(kind = kreal), intent(inout) :: c_ref_iso(nnod)
!
      integer(kind = kint) :: ncomp_org, ist_field
      integer(kind = kint) :: ifield, i_comp, ic
!
!
      ifield = iso_def%id_isosurf_data
      i_comp = iso_def%id_isosurf_comp
!
      ist_field  = istack_comp_nod(ifield-1) + 1
      ncomp_org = istack_comp_nod(ifield) - istack_comp_nod(ifield-1)
!
!
      if (ncomp_org .eq. 1) then
!$omp parallel
        call copy_nod_scalar_smp(nnod, d_nod(1,ist_field), c_ref_iso)
!$omp end parallel
!
      else if (ncomp_org .eq. 3) then
!$omp parallel
        if (i_comp.eq.0) then
          call cal_vector_magnitude(np_smp, nnod, istack_nod_smp,       &
     &         d_nod(1,ist_field), c_ref_iso)
!
        else if (i_comp.ge.1 .and. i_comp.le.3) then
          ic = ist_field+i_comp-1
          call copy_nod_scalar_smp(nnod, d_nod(1,ic), c_ref_iso)
!
        else if (i_comp.eq.11) then
          call cal_radial_comp_smp(np_smp, nnod, istack_nod_smp,        &
     &        d_nod(1,ist_field), c_ref_iso, xx(1,1), xx(1,2), xx(1,3), &
     &        radius, a_r)
        else if (i_comp.eq.12) then
          call cal_theta_comp_smp(np_smp, nnod, istack_nod_smp,         &
     &        d_nod(1,ist_field), c_ref_iso,                            &
     &        xx(1,1), xx(1,2), xx(1,3), radius, s_radius, a_r, a_s)
        else if (i_comp.eq.13) then
          call cal_phi_comp_smp(np_smp, nnod, istack_nod_smp,           &
     &        d_nod(1,ist_field), c_ref_iso,                            &
     &        xx(1,1), xx(1,2), s_radius, a_s)
        else if (i_comp.eq.14) then
          call cal_cylinder_r_comp_smp(np_smp, nnod, istack_nod_smp,    &
     &        d_nod(1,ist_field), c_ref_iso,                            &
     &        xx(1,1), xx(1,2), s_radius, a_s)
        end if
!$omp end parallel
!
      else if (ncomp_org .eq. 6) then
        if (i_comp.eq.0) then
!
!$omp parallel
          call cal_sym_tensor_magnitude(np_smp, nnod, istack_nod_smp,   &
     &        d_nod(1,ist_field), c_ref_iso)
!$omp end parallel
!
        else if (i_comp.ge.1 .and. i_comp.le.6) then
!
          ic = ist_field+i_comp-1
!$omp parallel
          call copy_nod_scalar_smp(nnod, d_nod(1,ic), c_ref_iso)
!$omp end parallel
!
        end if
      end if
!
!$omp parallel
      call subtruct_const_4_scalar_smp_ow                               &
     &   (nnod, c_ref_iso, iso_def%isosurf_value)
!$omp end parallel
!
      end subroutine set_constant_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_const_4_sections
