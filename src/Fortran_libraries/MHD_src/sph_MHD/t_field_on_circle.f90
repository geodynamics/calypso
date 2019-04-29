!>@file   t_field_on_circle.f90
!!@brief  module t_field_on_circle
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  field data on specific circle at (s,z)
!!
!!@verbatim
!!      subroutine sph_transfer_on_circle(sph_rj, rj_fld, cdat)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!        type(circle_fld_maker), intent(inout) :: cdat
!!      subroutine const_circle_point_global                            &
!!     &         (l_truncation, sph_rtp, sph_rj, cdat)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(circle_fld_maker), intent(inout) :: cdat
!!@endverbatim
!
      module t_field_on_circle
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
!
      use t_phys_data
      use t_circle_transform
      use t_FFT_selector
!
      implicit none
!
!
      type circle_fld_maker
        type(circle_transform_spetr) :: circ_spec
!>        Structure to make fields on circle
        type(fields_on_circle) :: circle
!>         Structure of field data on circle
        type(phys_data) :: d_circle
!>        Working structure for Fourier transform at mid-depth equator
!!@n      (Save attribute is necessary for Hitachi compiler for SR16000)
        type(working_FFTs) :: WK_circle_fft
      end type circle_fld_maker
!
      private :: collect_spectr_for_circle, set_circle_point_global
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sph_transfer_on_circle(sph_rj, rj_fld, cdat)
!
      use calypso_mpi
      use m_phys_constants
!
      use t_spheric_rj_data
      use t_phys_data
!
      use circle_transform_single
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      type(circle_fld_maker), intent(inout) :: cdat
!
      integer(kind = kint) :: ifld, icomp, m, nd
!
!
      call collect_spectr_for_circle(sph_rj%nidx_rj(2),                 &
     &    sph_rj%nidx_global_rj, sph_rj%idx_gl_1d_rj_j,                 &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%phys_name, rj_fld%d_fld,      &
     &    cdat%d_circle, cdat%circle)
!
!    spherical transfer
!
      if(my_rank .gt. 0) return
!
      do ifld = 1, cdat%d_circle%num_phys_viz
        icomp =  cdat%d_circle%istack_component(ifld-1) + 1
        if(cdat%d_circle%num_component(ifld) .eq. n_sym_tensor) then
          call circle_transfer_sym_tensor(icomp, cdat%circle,           &
     &        cdat%circ_spec, cdat%WK_circle_fft)
        else if(cdat%d_circle%num_component(ifld) .eq. n_vector) then
          call circle_transfer_vector(icomp, cdat%circle,               &
     &        cdat%circ_spec, cdat%WK_circle_fft)
        else
          call circle_transfer_scalar(icomp, cdat%circle,               &
     &        cdat%circ_spec, cdat%WK_circle_fft)
        end if
!
        do nd = 1, cdat%d_circle%num_component(ifld)
          do m = 1, cdat%circle%mphi_circle
            cdat%d_circle%d_fld(m,icomp+nd-1)                           &
     &         = cdat%circle%v_rtp_circle(m,nd)
          end do
        end do
      end do
!
      end subroutine sph_transfer_on_circle
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine const_circle_point_global                              &
     &         (l_truncation, sph_rtp, sph_rj, cdat)
!
      use t_spheric_rtp_data
      use t_spheric_rj_data
      use circle_transform_single
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) ::  sph_rj
!
      type(circle_fld_maker), intent(inout) :: cdat
!
!
      call alloc_circle_field                                           &
     &   (sph_rtp%nidx_rtp(3), sph_rj%nidx_global_rj(2),                &
     &    cdat%circle, cdat%d_circle)
      call alloc_circle_transform(l_truncation, cdat%circ_spec)
      call initialize_circle_transform(cdat%circle, cdat%circ_spec,     &
     &    cdat%WK_circle_fft)
      call set_circle_point_global                                      &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r,                     &
     &    cdat%circ_spec, cdat%circle)
!
      end subroutine const_circle_point_global
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_circle_point_global                                &
     &         (nri, radius_1d_rj_r, circ_spec, circle)
!
      integer(kind = kint), intent(in) ::  nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
      type(circle_transform_spetr), intent(in) :: circ_spec
!
      type(fields_on_circle), intent(inout) :: circle
!
      integer(kind = kint) :: kr
!
!
      circle%kr_gl_rcirc_in =  izero
      circle%kr_gl_rcirc_out = izero
      do kr = 1, nri - 1
        if(radius_1d_rj_r(kr) .eq. circ_spec%r_circle) then
          circle%kr_gl_rcirc_in =  kr
          circle%kr_gl_rcirc_out = izero
          circle%coef_gl_rcirc_in =  one
          circle%coef_gl_rcirc_out = zero
          exit
        end if
        if(radius_1d_rj_r(kr) .lt. circ_spec%r_circle                   &
     &      .and. radius_1d_rj_r(kr+1) .gt. circ_spec%r_circle) then
          circle%kr_gl_rcirc_in =  kr
          circle%kr_gl_rcirc_out = kr + 1
          circle%coef_gl_rcirc_in                                       &
     &                   = (radius_1d_rj_r(kr+1) - circ_spec%r_circle)  &
     &                    / (radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr))
          circle%coef_gl_rcirc_out = one - circle%coef_gl_rcirc_in
          exit
        end if
      end do
!
      end subroutine set_circle_point_global
!
! ----------------------------------------------------------------------
!
      subroutine collect_spectr_for_circle                              &
     &         (jmax, nidx_global_rj, idx_gl_1d_rj_j, nnod_rj,          &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj,         &
     &          phys_name_rj, d_rj, d_circle, circle)
!
      use calypso_mpi
!
      integer(kind = kint), intent(in) :: nnod_rj, jmax
      integer(kind = kint), intent(in) :: nidx_global_rj(2)
      integer(kind = kint), intent(in) :: idx_gl_1d_rj_j(jmax,3)
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &                  :: istack_phys_comp_rj(0:num_phys_rj)
      character (len=kchara), intent(in) :: phys_name_rj(num_phys_rj)
      real (kind=kreal), intent(in) :: d_rj(nnod_rj,ntot_phys_rj)
      type(phys_data), intent(in) :: d_circle
!
      type(fields_on_circle), intent(inout) :: circle
!
      integer(kind = kint) :: j, j_gl, i_in, i_ot, num, ncomp
      integer(kind = kint) :: ist_comp, jst_comp, nd, ifld, jfld
      integer(kind = kint_gl) :: num64
!
!
!    pickup spectrum for circle point
!
      do ifld = 1, d_circle%num_phys_viz
        ist_comp = d_circle%istack_component(ifld-1)
        do jfld = 1, num_phys_rj
          if(d_circle%phys_name(ifld) .eq. phys_name_rj(jfld)) then
            jst_comp = istack_phys_comp_rj(jfld-1)
            ncomp = istack_phys_comp_rj(jfld)                           &
     &             - istack_phys_comp_rj(jfld-1)
            if(iflag_debug .gt. 0) write(*,*)                           &
     &              trim(d_circle%phys_name(ifld)), ifld, jfld, ncomp
            do nd = 1, ncomp
              do j = 1, jmax
                j_gl = idx_gl_1d_rj_j(j,1)
                i_in = j + (circle%kr_gl_rcirc_in-1) *  jmax
                i_ot = j + (circle%kr_gl_rcirc_out-1) * jmax
!
                circle%d_rj_circ_lc(j_gl,ist_comp+nd)                   &
     &            = circle%coef_gl_rcirc_in * d_rj(i_in,jst_comp+nd)    &
     &             + circle%coef_gl_rcirc_out * d_rj(i_ot,jst_comp+nd)
              end do
            end do
            exit
          end if
        end do
      end do
!
!    collect data to rank 0
!
      num64 = d_circle%ntot_phys * (nidx_global_rj(2) + 1)
      if(my_rank .eq. 0) circle%d_rj_circle =   zero
      call calypso_mpi_reduce_real                                      &
     &   (circle%d_rj_circ_lc(0,1), circle%d_rj_circle(0,1), num64,     &
     &    MPI_SUM, 0)
!
      end subroutine collect_spectr_for_circle
!
! ----------------------------------------------------------------------
!
      end module t_field_on_circle
