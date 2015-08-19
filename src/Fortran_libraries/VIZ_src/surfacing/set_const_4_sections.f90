!set_const_4_sections.f90
!      module set_const_4_sections
!
!      Written by H. Matsui on June, 2006
!
!!      subroutine set_const_4_crossections                             &
!!     &          (num_psf, numnod, inod_smp_stack, xx, psf_list)
!!      subroutine set_const_4_isosurfaces(num_iso, numnod,             &
!!     &          inod_smp_stack, xx, radius, a_r, s_cyl, as_cyl,       &
!!     &          num_phys, ntot_phys, istack_ncomp, d_nod, iso_list)
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
     &          (num_psf, numnod, inod_smp_stack, xx, psf_list)
!
      use m_control_params_4_psf
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: num_psf, numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in) :: xx(numnod,3)
!
      type(sectioning_list), intent(inout):: psf_list(num_psf)
!
      integer(kind = kint) :: i_psf
!
!
      do i_psf = 1, num_psf
        if (id_section_method(i_psf) .gt. 0) then
          call set_constant_4_psf(i_psf, numnod, inod_smp_stack, xx,    &
     &        psf_list(i_psf)%ref_fld)
        end if
      end do
!
      end subroutine set_const_4_crossections
!
!  ---------------------------------------------------------------------
!
      subroutine set_const_4_isosurfaces(num_iso, numnod,               &
     &          inod_smp_stack, xx, radius, a_r, s_cyl, as_cyl,         &
     &          num_phys, ntot_phys, istack_ncomp, d_nod, iso_list)
!
      use m_control_params_4_iso
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: num_iso, numnod
      integer(kind = kint), intent(in) :: inod_smp_stack(0:np_smp)
      real(kind = kreal), intent(in)  :: xx(numnod,3)
      real(kind = kreal), intent(in)  :: radius(numnod)
      real(kind = kreal), intent(in)  :: a_r(numnod)
      real(kind = kreal), intent(in)  :: s_cyl(numnod)
      real(kind = kreal), intent(in)  :: as_cyl(numnod)
!
      integer(kind = kint), intent(in) :: num_phys, ntot_phys
      integer(kind = kint), intent(in) :: istack_ncomp(0:num_phys)
      real(kind = kreal), intent(in)  :: d_nod(numnod,ntot_phys)
!
      type(sectioning_list), intent(inout):: iso_list(num_iso)
!
      integer(kind = kint) :: i_iso
!
!
      do i_iso = 1, num_iso
        call set_constant_4_iso(i_iso, numnod, inod_smp_stack,          &
     &      xx, radius, a_r, s_cyl, as_cyl, num_phys, ntot_phys,        &
     &      istack_ncomp, d_nod, iso_list(i_iso)%ref_fld)
      end do
!
      end subroutine set_const_4_isosurfaces
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_constant_4_psf(i_psf, nnod, istack_nod_smp, xx,    &
     &          c_ref_psf)
!
      use m_control_params_4_psf
!
      integer(kind= kint), intent(in) :: i_psf
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: istack_nod_smp(0:np_smp)
      real(kind = kreal), intent(in) :: xx(nnod,3)
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
     &          =  const_psf(1,i_psf) * (xx(inod,1)*xx(inod,1))         &
     &           + const_psf(2,i_psf) * (xx(inod,2)*xx(inod,2))         &
     &           + const_psf(3,i_psf) * (xx(inod,3)*xx(inod,3))         &
     &           + const_psf(4,i_psf) * (xx(inod,1)*xx(inod,2))         &
     &           + const_psf(5,i_psf) * (xx(inod,2)*xx(inod,3))         &
     &           + const_psf(6,i_psf) * (xx(inod,3)*xx(inod,1))         &
     &           + const_psf(7,i_psf) *  xx(inod,1)                     &
     &           + const_psf(8,i_psf) *  xx(inod,2)                     &
     &           + const_psf(9,i_psf) *  xx(inod,3)                     &
     &           + const_psf(10,i_psf)
        end do
      end do
!$omp end parallel do
!
      end subroutine set_constant_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_constant_4_iso(i_iso, nnod, istack_nod_smp,        &
     &          xx, radius, a_r, s_radius, a_s, num_fld, ntot_comp,     &
     &          istack_comp_nod, d_nod, c_ref_iso)
!
      use m_control_params_4_iso
!
      use mag_of_field_smp
      use cvt_xyz_vector_2_sph_smp
      use cvt_xyz_vector_2_cyl_smp
      use subtract_const_smp
!
      use copy_field_smp
!
      integer(kind = kint), intent(in) :: i_iso
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
      ifield = id_isosurf_data(i_iso)
      i_comp = id_isosurf_comp(i_iso)
!
      ist_field  = istack_comp_nod(ifield-1) + 1
      ncomp_org = istack_comp_nod(ifield) - istack_comp_nod(ifield-1)
!
!
      if (ncomp_org .eq. 1) then
!$omp parallel
        call copy_nod_scalar_smp(np_smp, nnod, istack_nod_smp,          &
     &      d_nod(1,ist_field), c_ref_iso)
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
          call copy_nod_scalar_smp(np_smp, nnod, istack_nod_smp,        &
     &        d_nod(1,ic), c_ref_iso)
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
          call copy_nod_scalar_smp(np_smp, nnod, istack_nod_smp,        &
     &        d_nod(1,ic), c_ref_iso)
!$omp end parallel
!
        end if
      end if
!
!$omp parallel
      call subtruct_const_4_scalar_smp_ow(np_smp, nnod, istack_nod_smp, &
     &    c_ref_iso, isosurf_value(i_iso))
!$omp end parallel
!
      end subroutine set_constant_4_iso
!
!  ---------------------------------------------------------------------
!
      end module set_const_4_sections
