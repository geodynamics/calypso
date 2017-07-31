!>@file   sph_MHD_circle_transform.f90
!!@brief  module sph_MHD_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  field data on specific circle at (s,z)
!!
!!@verbatim
!!      subroutine sph_transfer_on_circle(sph_rj, rj_fld)
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!        type(phys_data), intent(in) :: rj_fld
!!      subroutine const_circle_point_global                            &
!!     &         (l_truncation, sph_rtp, sph_rj)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) ::  sph_rj
!!@endverbatim
!
      module sph_MHD_circle_transform
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_field_on_circle
!
      implicit none
!
      private :: collect_spectr_for_circle, set_circle_point_global
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sph_transfer_on_circle(sph_rj, rj_fld)
!
      use calypso_mpi
      use m_phys_constants
      use m_circle_transform
!
      use t_spheric_rj_data
      use t_phys_data
!
      use circle_transform_single
!
      type(sph_rj_grid), intent(in) ::  sph_rj
      type(phys_data), intent(in) :: rj_fld
!
      integer(kind = kint) :: ifld, icomp, m, nd
!
!
      call collect_spectr_for_circle(sph_rj%nidx_rj(2),                 &
     &    sph_rj%nidx_global_rj, sph_rj%idx_gl_1d_rj_j,                 &
     &    rj_fld%n_point, rj_fld%num_phys, rj_fld%ntot_phys,            &
     &    rj_fld%istack_component, rj_fld%phys_name, rj_fld%d_fld)
!
!    spherical transfer
!
      if(my_rank .gt. 0) return
!
      do ifld = 1, d_circle%num_phys_viz
        icomp =  d_circle%istack_component(ifld-1) + 1
        if(d_circle%num_component(ifld) .eq. n_sym_tensor) then
          call circle_transfer_sym_tensor                               &
     &       (d_rj_circle(0,icomp), v_rtp_circle(1,1),                  &
     &        vrtm_mag(0,icomp), vrtm_phase(0,icomp) )
        else if(d_circle%num_component(ifld) .eq. n_vector) then
          call circle_transfer_vector                                   &
     &       (d_rj_circle(0,icomp), v_rtp_circle(1,1),                  &
     &        vrtm_mag(0,icomp), vrtm_phase(0,icomp) )
        else
          call circle_transfer_scalar                                   &
     &       (d_rj_circle(0,icomp), v_rtp_circle(1,1),                  &
     &        vrtm_mag(0,icomp), vrtm_phase(0,icomp) )
        end if
!
        do nd = 1, d_circle%num_component(ifld)
          do m = 1, mphi_circle
            d_circle%d_fld(m,icomp+nd-1) = v_rtp_circle(m,nd)
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
     &         (l_truncation, sph_rtp, sph_rj)
!
      use t_spheric_rtp_data
      use t_spheric_rj_data
      use m_circle_transform
      use circle_transform_single
!
      integer(kind = kint), intent(in) :: l_truncation
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) ::  sph_rj
!
!
      call allocate_circle_field                                        &
     &   (sph_rtp%nidx_rtp(3), sph_rj%nidx_global_rj(2))
      call initialize_circle_transform(l_truncation,                    &
     &    s_circle, z_circle)
      call set_circle_point_global                                      &
     &   (sph_rj%nidx_rj(1), sph_rj%radius_1d_rj_r)
!
      end subroutine const_circle_point_global
!
! ----------------------------------------------------------------------
!
      subroutine set_circle_point_global(nri, radius_1d_rj_r)
!
      use m_circle_transform
!
      integer(kind = kint), intent(in) ::  nri
      real(kind = kreal), intent(in) :: radius_1d_rj_r(nri)
!
      integer(kind = kint) :: kr
!
!
      kr_gl_rcirc_in =  izero
      kr_gl_rcirc_out = izero
      do kr = 1, nri - 1
        if(radius_1d_rj_r(kr) .eq. r_circle) then
          kr_gl_rcirc_in =  kr
          kr_gl_rcirc_out = izero
          coef_gl_rcirc_in =  one
          coef_gl_rcirc_out = zero
          exit
        end if
        if(radius_1d_rj_r(kr) .lt. r_circle                             &
     &      .and. radius_1d_rj_r(kr+1) .gt. r_circle) then
          kr_gl_rcirc_in =  kr
          kr_gl_rcirc_out = kr + 1
          coef_gl_rcirc_in =  (radius_1d_rj_r(kr+1) - r_circle)         &
     &                    / (radius_1d_rj_r(kr+1) - radius_1d_rj_r(kr))
          coef_gl_rcirc_out = one - coef_gl_rcirc_in
          exit
        end if
      end do
!
      end subroutine set_circle_point_global
!
! ----------------------------------------------------------------------
!
      subroutine collect_spectr_for_circle                              &
     &         (jmax, nidx_global_rj, idx_gl_1d_rj_j,                   &
     &          nnod_rj, num_phys_rj, ntot_phys_rj,                     &
     &          istack_phys_comp_rj, phys_name_rj, d_rj)
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
!
      integer(kind = kint) :: j, j_gl, i_in, i_ot, num, ncomp
      integer(kind = kint) :: ist_comp, jst_comp, nd, ifld, jfld
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
                i_in = j + (kr_gl_rcirc_in-1) *  jmax
                i_ot = j + (kr_gl_rcirc_out-1) * jmax
!
                d_rj_circ_lc(j_gl,ist_comp+nd)                          &
     &                      = coef_gl_rcirc_in * d_rj(i_in,jst_comp+nd) &
     &                     + coef_gl_rcirc_out * d_rj(i_ot,jst_comp+nd)
              end do
            end do
            exit
          end if
        end do
      end do
!
!    collect data to rank 0
!
      num = d_circle%ntot_phys * (nidx_global_rj(2) + 1)
      if(my_rank .eq. 0) d_rj_circle =   zero
      call MPI_Reduce(d_rj_circ_lc(0,1), d_rj_circle(0,1), num,         &
     &    CALYPSO_REAL, MPI_SUM, izero, CALYPSO_COMM, ierr_MPI)
!
      end subroutine collect_spectr_for_circle
!
! ----------------------------------------------------------------------
!
      end module sph_MHD_circle_transform
