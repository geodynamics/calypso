!>@file   sph_MHD_circle_transform.f90
!!@brief  module sph_MHD_circle_transform
!!
!!@author H. Matsui
!!@date Programmed on June., 2011
!
!>@brief  field data on specific circle at (s,z)
!!
!!@verbatim
!!      subroutine sph_transfer_on_circle
!!      subroutine set_circle_point_global
!!@endverbatim
!
      module sph_MHD_circle_transform
!
      use m_precision
!
      use m_constants
      use m_machine_parameter
      use m_spheric_parameter
      use m_field_on_circle
!
      implicit none
!
!      private :: collect_spectr_for_circle
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine sph_transfer_on_circle
!
      use m_phys_constants
      use m_parallel_var_dof
      use m_sph_phys_address
      use m_circle_transform
!
      use circle_transform_single
!
      integer(kind = kint) :: ifld, icomp, m, nd
!
!
      call collect_spectr_for_circle
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
      subroutine set_circle_point_global
!
      use m_circle_transform
      use circle_transform_single
!
      integer(kind = kint) :: kr
!
!
      call allocate_circle_field
      call initialize_circle_transform(l_truncation,                    &
     &    s_circle, z_circle)
!
      kr_gl_rcirc_in =  izero
      kr_gl_rcirc_out = izero
      do kr = 1, nidx_rj(1) - 1
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
      subroutine collect_spectr_for_circle
!
      use m_parallel_var_dof
      use m_sph_spectr_data
      use m_sph_phys_address
!
      integer(kind = kint) :: j, j_gl, i_in, i_ot, num
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
            if(iflag_debug .gt. 0) write(*,*)                           &
     &              trim(d_circle%phys_name(ifld)),                     &
     &              ifld, jfld, num_phys_comp_rj(jfld)
            do nd = 1, num_phys_comp_rj(jfld)
              do j = 1, nidx_rj(2)
                j_gl = idx_gl_1d_rj_j(j,1)
                i_in = j + (kr_gl_rcirc_in-1) *  nidx_rj(2)
                i_ot = j + (kr_gl_rcirc_out-1) * nidx_rj(2)
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
     &    MPI_DOUBLE_PRECISION, MPI_SUM, izero, SOLVER_COMM, ierr)
!
      end subroutine collect_spectr_for_circle
!
! ----------------------------------------------------------------------
!
      end module sph_MHD_circle_transform
