!set_field_comp_for_viz.f90
!      module set_field_comp_for_viz
!
!        programmed by H.Matsui on May. 2006
!
!!      subroutine check_field_4_viz(num_nod_phys, phys_nod_name,       &
!!     &          n_field_ctl, field_name, num_field, num_field_vis)
!!      subroutine set_components_4_viz(num_nod_phys, phys_nod_name,    &
!!     &          n_field_ctl, field_name, comp_name, num_field,        &
!!     &          ifield, icomp, ncomp, ncomp_org, rst_name)
!!      subroutine set_one_component_4_viz(num_nod_phys, phys_nod_name, &
!!     &          field_name, comp_name, ifield, icomp, ncomp,          &
!!     &          ncomp_org, rst_name)
!!
!!      subroutine count_total_comps_4_viz(psf_fld)
!
      module set_field_comp_for_viz
!
      use m_precision
!
      implicit  none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine check_field_4_viz(num_nod_phys, phys_nod_name,         &
     &          n_field_ctl, field_name, num_field, num_field_vis)
!
      integer(kind = kint), intent(in) :: num_nod_phys, n_field_ctl
!
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
      character(len=kchara), intent(in) :: field_name(n_field_ctl)
!
      integer(kind = kint), intent(inout) :: num_field, num_field_vis
!
      integer(kind = kint) :: i, id
!
!
      num_field = 0
      do i = 1, n_field_ctl
        do id = 1, num_nod_phys
          if ( field_name(i) .eq. phys_nod_name(id) ) then
            num_field = num_field + 1
            exit
          end if
        end do
      end do
      num_field_vis = num_field
!
      end subroutine check_field_4_viz
!
!  ---------------------------------------------------------------------
!
      subroutine set_components_4_viz(num_nod_phys, phys_nod_name,      &
     &          n_field_ctl, field_name, comp_name, num_field,          &
     &          ifield, icomp, ncomp, ncomp_org, rst_name)
!
      use set_components_flags
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      integer(kind = kint), intent(in) :: n_field_ctl
      integer(kind = kint), intent(in) :: num_field
      character(len=kchara), intent(in) :: field_name(n_field_ctl)
      character(len=kchara), intent(in) :: comp_name(n_field_ctl)
!
      integer(kind = kint), intent(inout) :: ifield(num_field)
      integer(kind = kint), intent(inout) :: icomp(num_field)
      integer(kind = kint), intent(inout) :: ncomp(num_field)
      integer(kind = kint), intent(inout) :: ncomp_org(num_field)
      character(len=kchara), intent(inout) :: rst_name(num_field)
!
      integer(kind = kint) :: i, id, icou
!
!
      icou = 0
      do i = 1, n_field_ctl
        do id = 1, num_nod_phys
          if ( field_name(i) .eq. phys_nod_name(id) ) then
!
            icou = icou + 1
            ifield(icou) = id
!
            call s_set_components_flags( comp_name(i), field_name(i),   &
     &          icomp(icou), ncomp(icou), ncomp_org(icou),              &
     &          rst_name(icou))
            exit
          end if
        end do
      end do
!
      end subroutine set_components_4_viz
!
!  ---------------------------------------------------------------------
!
      subroutine set_one_component_4_viz(num_nod_phys, phys_nod_name,   &
     &          field_name, comp_name, ifield, icomp, ncomp,            &
     &          ncomp_org, rst_name)
!
      use set_components_flags
!
      integer(kind = kint), intent(in) :: num_nod_phys
      character(len=kchara), intent(in) :: phys_nod_name(num_nod_phys)
!
      character(len=kchara), intent(in) :: field_name
      character(len=kchara), intent(in) :: comp_name
!
      integer(kind = kint), intent(inout) :: ifield
      integer(kind = kint), intent(inout) :: icomp
      integer(kind = kint), intent(inout) :: ncomp
      integer(kind = kint), intent(inout) :: ncomp_org
      character(len=kchara), intent(inout) :: rst_name
!
      integer(kind = kint) :: id
!
!
      do id = 1, num_nod_phys
        if ( field_name .eq. phys_nod_name(id) ) then
          ifield = id
!
          call s_set_components_flags(comp_name, field_name,            &
     &        icomp, ncomp, ncomp_org, rst_name)
          exit
        end if
      end do
!
      end subroutine set_one_component_4_viz
!
!  ---------------------------------------------------------------------
!
      subroutine count_total_comps_4_viz(psf_fld)
!
      use t_phys_data
!
      type(phys_data), intent(inout) :: psf_fld
!
!
      integer(kind = kint) :: j
!
!
      psf_fld%ntot_phys = 0
      do j = 1, psf_fld%num_phys
        psf_fld%ntot_phys = psf_fld%ntot_phys                           &
     &                     + psf_fld%num_component(j)
      end do
      psf_fld%ntot_phys_viz = psf_fld%ntot_phys
!
      end subroutine count_total_comps_4_viz
!
!  ---------------------------------------------------------------------
!
      end module set_field_comp_for_viz
