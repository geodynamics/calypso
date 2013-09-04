!r_interpolate_sph_data.f90
!     module r_interpolate_sph_data
!
!      Written by H. Matsui on Sep., 2011
!
!      subroutine deallocate_original_sph_data
!
!      subroutine copy_cmb_icb_radial_point
!      subroutine set_cmb_icb_radial_point(cmb_r_grp, icb_r_grp)
!      subroutine set_sph_magne_address
!      subroutine input_old_rj_sph_trans(my_rank)
!
!      subroutine r_interpolate_sph_rst_from_IO
!         (substitution for set_sph_restart_from_IO)
!      subroutine r_interpolate_sph_fld_from_IO
!         (substitution for set_rj_phys_data_from_IO)
!      subroutine set_poloidal_b_by_gauss_coefs
!
      module r_interpolate_sph_data
!
      use m_precision
!
      use m_constants
      use m_parallel_var_dof
      use m_spheric_parameter
!
      implicit  none
!
!
      integer(kind = kint) :: kr_inside, kr_outside
!
      integer(kind = kint) :: nri_org, n_rj_org
      integer(kind = kint), allocatable :: k_inter(:,:)
      real(kind = kreal), allocatable :: rcoef_inter(:,:)
!
      real(kind = kreal), allocatable :: r_org(:)
!
      integer(kind = kint) :: ntot_phys_rj_itp
      real(kind = kreal), allocatable :: d_rj_org(:,:)
      real(kind = kreal), allocatable :: d_rj_itp(:,:)
!
      private :: allocate_original_sph_data
      private :: copy_original_sph_rj_from_IO, const_radial_itp_table
      private :: set_org_rj_phys_data_from_IO, r_interpolate_sph_vector
!
!  -------------------------------------------------------------------
!
      contains
!
!  -------------------------------------------------------------------
!
      subroutine copy_cmb_icb_radial_point
!
!
      kr_outside = nlayer_CMB
      kr_inside =  nlayer_ICB
!
      end subroutine copy_cmb_icb_radial_point
!
!  -------------------------------------------------------------------
!
      subroutine set_cmb_icb_radial_point(cmb_r_grp, icb_r_grp)
!
      use m_group_data_sph_specr
!
      character(len = kchara), intent(in) :: cmb_r_grp, icb_r_grp
      integer(kind = kint) :: igrp, inum
!
!
      kr_outside = 0
      do igrp = 1, num_radial_grp_rj
        if(name_radial_grp_rj(igrp) .eq. cmb_r_grp) then
          inum = istack_radial_grp_rj(igrp-1) + 1
          kr_outside = item_radial_grp_rj(inum)
          exit
        end if
      end do
!
      kr_inside = 0
      do igrp = 1, num_radial_grp_rj
        if(name_radial_grp_rj(igrp) .eq. icb_r_grp) then
          inum = istack_radial_grp_rj(igrp-1) + 1
          kr_inside = item_radial_grp_rj(inum)
          exit
        end if
      end do
!
      end subroutine set_cmb_icb_radial_point
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine allocate_original_sph_data
!
!
      allocate(r_org(nri_org))
      allocate(k_inter(nri_org,2))
      allocate(rcoef_inter(nri_org,2))
!
      allocate(d_rj_org(n_rj_org,6))
!
      k_inter = izero
      r_org = zero
      rcoef_inter = zero
      d_rj_org =    zero
!
      end subroutine allocate_original_sph_data
!
!  -------------------------------------------------------------------
!
      subroutine deallocate_original_sph_data
!
      deallocate(r_org, d_rj_org)
      deallocate(k_inter, rcoef_inter)
!
      end subroutine deallocate_original_sph_data
!
!  -------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_sph_magne_address
!
      use m_phys_labels
      use m_sph_phys_address
      use m_sph_spectr_data
!
      integer(kind = kint) :: i
!
      do i = 1, num_phys_rj
        if(phys_name_rj(i) .eq. fhd_magne) then
          ipol%i_magne = istack_phys_comp_rj(i-1) + 1
          exit
        end if
      end do
!
      end subroutine set_sph_magne_address
!
!  -------------------------------------------------------------------
!
      subroutine input_old_rj_sph_trans(my_rank)
!
      use sph_file_IO_select
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      call sel_read_org_spectr_rj_file(my_rank)
      call copy_original_sph_rj_from_IO
!
      call const_radial_itp_table
!
      end subroutine input_old_rj_sph_trans
!
!  -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_rst_from_IO
!
      use m_phys_labels
      use m_sph_phys_address
      use m_sph_spectr_data
      use m_field_data_IO
      use extend_potential_field
!
      integer(kind = kint) :: i_fld, j_fld
!
!
      do i_fld = 1, ntot_phys_rj
        do j_fld = 1, num_phys_data_IO
          if (phys_name_rj(i_fld) .eq. phys_data_name_IO(j_fld)) then
            if     (phys_name_rj(i_fld) .eq. fhd_velo                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_vort                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_press                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_temp                   &
     &         .or. phys_name_rj(i_fld) .eq. fhd_light                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_magne                  &
     &         .or. phys_name_rj(i_fld) .eq. fhd_mag_potential          &
     &         .or. phys_name_rj(i_fld) .eq. fhd_entropy                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_mom                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_uxb                &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_heat               &
     &         .or. phys_name_rj(i_fld) .eq. fhd_pre_composit           &
     &         .or. phys_name_rj(i_fld) .eq. fhd_heat_source            &
     &         .or. phys_name_rj(i_fld) .eq. fhd_light_source           &
     &         .or. phys_name_rj(i_fld) .eq. fhd_entropy_source         &
     &         ) then
              call set_org_rj_phys_data_from_IO(j_fld)
              call r_interpolate_sph_vector(i_fld)
              exit
            end if
          end if
        end do
      end do
!
      if (ipol%i_magne .gt. 0) then
        call ext_outside_potential(ipol%i_magne, kr_outside)
        call ext_inside_potential(ipol%i_magne, kr_inside)
      end if
!
      end subroutine r_interpolate_sph_rst_from_IO
!
! -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_fld_from_IO
!
      use m_sph_phys_address
      use m_sph_spectr_data
      use m_field_data_IO
      use extend_potential_field
!
      integer(kind = kint) ::  i_fld, j_fld
!
!
      do i_fld = 1, ntot_phys_rj
        do j_fld = 1, num_phys_data_IO
          if (phys_name_rj(i_fld) .eq. phys_data_name_IO(j_fld)) then
            call set_org_rj_phys_data_from_IO(j_fld)
            call r_interpolate_sph_vector(i_fld)
            exit
          end if
        end do
      end do
!
      if (ipol%i_magne .gt. 0) then
        call ext_outside_potential(ipol%i_magne, kr_outside)
        call ext_inside_potential(ipol%i_magne, kr_inside)
      end if
!
      end subroutine r_interpolate_sph_fld_from_IO
!
! -----------------------------------------------------------------------
!  -------------------------------------------------------------------
!
      subroutine set_poloidal_b_by_gauss_coefs
!
      use m_sph_phys_address
      use m_sph_spectr_data
      use m_global_gauss_coefs
      use extend_potential_field
!
!
!      write(*,*) ' ipol%i_magne', ipol%i_magne, kr_outside, kr_inside
      if (ipol%i_magne .gt. 0) then
        call gauss_to_poloidal_out(ipol%i_magne, kr_outside,            &
     &      ltr_w, r_gauss, w_gauss)
        call gauss_to_poloidal_in(ipol%i_magne,  kr_inside,             &
     &      ltr_w, r_gauss, w_gauss)
      end if
!
      end subroutine set_poloidal_b_by_gauss_coefs
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_original_sph_rj_from_IO
!
      use m_node_id_spherical_IO
      use m_comm_data_IO
      use m_group_data_sph_specr_IO
!
!
      if(sph_rank_rj(1).ne.sph_rank_IO(1)                               &
     &       .or. sph_rank_rj(2).ne.sph_rank_IO(2)) then
        call parallel_abort(1,'rj rank ID is wrong')
      end if
!
      if(nidx_global_rj(2) .ne. nidx_gl_sph_IO(2)) then
        call parallel_abort(1,'number of local mode is wrong')
      end if
      if(l_truncation .ne. ltr_gl_IO) then
        call parallel_abort(1,'truncation is wrong')
      end if
!
      if(ist_rj(2).ne.ist_sph_IO(2)) then
        call parallel_abort(1,'start point of harminics is wrong')
      end if
      if(ied_rj(2).ne.ied_sph_IO(2)) then
        call parallel_abort(1,'end point of harminics is wrong')
      end if
!
      n_rj_org = nnod_sph_IO
      nri_org =  nidx_sph_IO(1)
!
      call allocate_original_sph_data
!
      r_org(1:n_rj_org) =   r_gl_1_IO(1:n_rj_org)
!
      call deallocate_nod_id_sph_IO
      call deallocate_idx_sph_1d1_IO
      call deallocate_idx_sph_1d2_IO
!
!
      call deallocate_import_item_IO
      call deallocate_neib_domain_IO
!
      call deallocate_rj_r_grp_IO_item
      call deallocate_rj_j_grp_IO_item
!
      end subroutine copy_original_sph_rj_from_IO
!
! ----------------------------------------------------------------------
!
      subroutine const_radial_itp_table
!
      integer(kind = kint) :: kst, k1, k2
!
!
      kst = 1
      do k1 = 1, nidx_rj(1)
        if(radius_1d_rj_r(k1) .lt. r_org(1)) then
          kr_inside = k1+1
          k_inter(k1,1) = 0
          k_inter(k1,2) = 0
          rcoef_inter(k1,1) = zero
          rcoef_inter(k1,2) = one
        else if(radius_1d_rj_r(k1) .gt. r_org(nri_org)) then
          kr_outside = k1-1
          exit
        else
          do k2 = kst, nri_org-1
            if(radius_1d_rj_r(k1).ge.r_org(k2)                          &
     &        .and. radius_1d_rj_r(k1).le.r_org(k2+1)) then
              k_inter(k1,1) = k2
              k_inter(k1,2) = k2 + 1
              rcoef_inter(k1,1) = (radius_1d_rj_r(k1) - r_org(k2))      &
     &                           / (r_org(k2+1) - r_org(k2))
              rcoef_inter(k1,2) = (r_org(k2+1) - radius_1d_rj_r(k1))    &
     &                           / (r_org(k2+1) - r_org(k2))
              kst = k2
              exit
            end if
          end do
        end if
      end do
      do k1 = kr_outside+1, nidx_rj(1)
         k_inter(k1,1) = nri_org+1
         k_inter(k1,2) = nri_org+1
         rcoef_inter(k1,1) = one
         rcoef_inter(k1,2) = zero
      end do
!
      end subroutine const_radial_itp_table
!
!  -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_org_rj_phys_data_from_IO(j_fld)
!
      use m_field_data_IO
!
      integer(kind = kint), intent(in) :: j_fld
      integer(kind = kint) :: jst, nd
!
!
      jst = istack_phys_comp_IO(j_fld-1)
      if(num_phys_comp_IO(j_fld) .eq. 3) then
        d_rj_org(1:n_rj_org,1) = phys_data_IO(1:n_rj_org,jst+1)
        d_rj_org(1:n_rj_org,2) = phys_data_IO(1:n_rj_org,jst+3)
        d_rj_org(1:n_rj_org,3) = phys_data_IO(1:n_rj_org,jst+2)
      else
        do nd = 1, num_phys_comp_IO(j_fld)
          d_rj_org(1:n_rj_org,nd) = phys_data_IO(1:n_rj_org,jst+nd)
        end do
      end if
!
      end subroutine set_org_rj_phys_data_from_IO
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine r_interpolate_sph_vector(i_fld)
!
      use m_sph_spectr_data
!
      integer(kind = kint), intent(in) :: i_fld
      integer(kind = kint) :: i_comp, ist, ied, inod, k, j, nd, i1, i2
!
!
!$omp parallel private(nd,i_comp,ist,ied,inod)
      do nd = 1, num_phys_comp_rj(i_fld)
        i_comp = nd + istack_phys_comp_rj(i_fld-1)
        ist = 1
        ied = (kr_inside-1) * nidx_rj(2)
!$omp do private(inod)
        do inod = 1, kr_inside-1
            d_rj(inod,i_comp) = zero
        end do
!$omp end do nowait
!
!$omp do private(inod,k,j,i1,i2)
        do k = kr_inside, kr_outside
          do j = 1, nidx_rj(2)
            inod = j + (k-1) * nidx_rj(2)
            i1 = j + (k_inter(k,1)-1) * nidx_rj(2)
            i2 = j + (k_inter(k,2)-1) * nidx_rj(2)
            d_rj(i_comp,i_comp) = rcoef_inter(k,1)*d_rj_org(i1,nd)      &
     &                         +  rcoef_inter(k,1)*d_rj_org(i2,nd)
          end do
        end do
!$omp end do nowait
!
        ist = 1 + kr_outside * nidx_rj(2)
        ied = nidx_rj(1) * nidx_rj(2)
!$omp do private(inod)
        do inod = 1, kr_inside-1
          d_rj(inod,i_comp) = zero
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine r_interpolate_sph_vector
!
!  -------------------------------------------------------------------
!
      end module r_interpolate_sph_data
