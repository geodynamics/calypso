!>@file   parallel_assemble_sph.f90
!!@brief  module parallel_assemble_sph
!!
!!@author H. Matsui
!!@date Programmed in Oct., 2007
!
!>@brief Construct spectrum data for new spectrum domain
!!
!!@verbatim
!!      subroutine alloc_each_mode_tbl_4_assemble(org_sph, j_table)
!!      subroutine dealloc_mode_table_4_assemble(j_table)
!!
!!      subroutine set_mode_table_4_assemble(org_sph, new_sph, j_table)
!!      subroutine copy_field_data_sph_assemble(org_sph, new_sph,       &
!!     &          j_table, ntot_phys_rj, d_rj_org, d_rj_tmp)
!!      subroutine r_itp_field_data_sph_assemble(org_sph, new_sph,      &
!!     &          r_itp, j_table, ntot_phys_rj, d_rj_org, d_rj_new)
!!      subroutine copy_field_data_sph_center(org_sph, new_sph, j_table,&
!!     &          ntot_phys_rj, d_rj_org, d_rj)
!!      subroutine mul_sph_magne(b_sph_ratio, nnod_rj,                  &
!!     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj,       &
!!     &          phys_name_rj, d_rj)
!!@endverbatim
!!
!!@param   istep  TIme step number
!
      module parallel_assemble_sph
!
      use m_precision
      use m_machine_parameter
      use m_constants
!
      implicit none
!
      type rj_assemble_tbl
        integer(kind = kint), pointer :: j_org_to_new(:)
        integer(kind = kint) :: icenter
      end type rj_assemble_tbl
!
! -------------------------------------------------------------------
!
      contains
!
! -------------------------------------------------------------------
!
      subroutine alloc_each_mode_tbl_4_assemble(org_sph, j_table)
!
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: org_sph
      type(rj_assemble_tbl), intent(inout) :: j_table
      integer(kind = kint) ::  num
!
!
      j_table%icenter = 0
!
      num = org_sph%sph_rj%nidx_rj(2)
      allocate(j_table%j_org_to_new(num))
      if(num .gt. 0) j_table%j_org_to_new = 0
!
      end subroutine alloc_each_mode_tbl_4_assemble
!
! -------------------------------------------------------------------
!
      subroutine dealloc_mode_table_4_assemble(j_table)
!
      type(rj_assemble_tbl), intent(inout) :: j_table
!
!
      deallocate(j_table%j_org_to_new)
!
      end subroutine dealloc_mode_table_4_assemble
!
! -------------------------------------------------------------------
! -------------------------------------------------------------------
!
      subroutine set_mode_table_4_assemble(org_sph, new_sph, j_table)
!
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: org_sph
      type(sph_grids), intent(in) :: new_sph
      type(rj_assemble_tbl), intent(inout) :: j_table
!
      integer(kind = kint) :: j_org, j_gl, j_new
      integer(kind = 4) :: l_gl, m_gl
!
!
      do j_org = 1, org_sph%sph_rj%nidx_rj(2)
        j_gl =     org_sph%sph_rj%idx_gl_1d_rj_j(j_org,1)
        l_gl = int(org_sph%sph_rj%idx_gl_1d_rj_j(j_org,2))
        m_gl = int(org_sph%sph_rj%idx_gl_1d_rj_j(j_org,3))
        j_new = find_local_sph_mode_address_t                           &
     &          (new_sph%sph_rj, l_gl, m_gl)
        if(j_new .gt. 0) then
          j_table%j_org_to_new(j_org) = j_new
        else
          j_table%j_org_to_new(j_org) = 0
        end if
      end do
!
      if(new_sph%inod_rj_center .gt. 0                                  &
      &   .and. org_sph%inod_rj_center .gt. 0) then
        j_table%icenter = new_sph%inod_rj_center
      else
        j_table%icenter = 0
      end if
!
      end subroutine set_mode_table_4_assemble
!
! -------------------------------------------------------------------
!
      subroutine copy_field_data_sph_assemble(org_sph, new_sph,         &
     &          j_table, ntot_phys_rj, d_rj_org, d_rj)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: ntot_phys_rj
      type(sph_grids), intent(in) :: org_sph
      type(sph_grids), intent(in) :: new_sph
      type(rj_assemble_tbl), intent(in) :: j_table
      real(kind = kreal), intent(in)                                    &
     &                :: d_rj_org(org_sph%sph_rj%nnod_rj,ntot_phys_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: d_rj(new_sph%sph_rj%nnod_rj,ntot_phys_rj)
!
!
      integer(kind = kint) :: nd, j_org, j_new, kr
      integer(kind = kint) :: inod_org, inod_new
!
!!$omp parallel private(nd)
      do nd = 1, ntot_phys_rj
!!$omp do private(j_org,j_new,kr,inod_org,inod_new)
        do j_org = 1, org_sph%sph_rj%nidx_rj(2)
          j_new = j_table%j_org_to_new(j_org)
          if(j_new .le. 0) cycle
!
          do kr = 1, org_sph%sph_rj%nidx_rj(1)
            inod_org = j_org + (kr - 1) * org_sph%sph_rj%nidx_rj(2)
            inod_new = j_new + (kr - 1) * new_sph%sph_rj%nidx_rj(2)
            d_rj(inod_new,nd) = d_rj_org(inod_org,nd)
          end do
        end do
!!$omp end do
      end do
!!$omp end parallel
!
      end subroutine copy_field_data_sph_assemble
!
! -------------------------------------------------------------------
!
      subroutine copy_field_data_sph_center(org_sph, new_sph, j_table,  &
     &          ntot_phys_rj, d_rj_org, d_rj)
!
      use t_spheric_parameter
!
      integer(kind = kint), intent(in) :: ntot_phys_rj
      type(sph_grids), intent(in) :: org_sph
      type(sph_grids), intent(in) :: new_sph
      type(rj_assemble_tbl), intent(in) :: j_table
      real(kind = kreal), intent(in)                                    &
     &                :: d_rj_org(org_sph%sph_rj%nnod_rj,ntot_phys_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: d_rj(new_sph%sph_rj%nnod_rj,ntot_phys_rj)
!
      integer(kind = kint) :: nd, inod_org, inod_new
!
!
      if(j_table%icenter .eq. 0) return
!
!$omp parallel
      do nd = 1, ntot_phys_rj
        inod_org = org_sph%inod_rj_center
        inod_new = j_table%icenter
        d_rj(inod_new,nd) = d_rj_org(inod_org,nd)
      end do
!$omp end parallel
!
      end subroutine copy_field_data_sph_center
!
! -------------------------------------------------------------------
!
      subroutine r_itp_field_data_sph_assemble(org_sph, new_sph,        &
     &          r_itp, j_table, ntot_phys_rj, d_rj_org, d_rj_new)
!
      use t_spheric_parameter
      use r_interpolate_marged_sph
!
      integer(kind = kint), intent(in) :: ntot_phys_rj
      type(sph_grids), intent(in) :: org_sph
      type(sph_grids), intent(in) :: new_sph
      type(rj_assemble_tbl), intent(in) :: j_table
      type(sph_radial_itp_data), intent(in) :: r_itp
!
      real(kind = kreal), intent(in)                                    &
     &                :: d_rj_org(org_sph%sph_rj%nnod_rj,ntot_phys_rj)
!
      real(kind = kreal), intent(inout)                                 &
     &                :: d_rj_new(new_sph%sph_rj%nnod_rj,ntot_phys_rj)
!
!
      integer(kind = kint) :: nd, j_org, j_new, kr, kr_in, kr_out
      integer(kind = kint) :: inod_in, inod_out, inod_new
!
!$omp parallel private(nd)
      do nd = 1, ntot_phys_rj
!$omp do private(j_org,j_new,kr,kr_in,kr_out,inod_in,inod_out,inod_new)
        do j_org = 1, org_sph%sph_rj%nidx_rj(2)
          j_new = j_table%j_org_to_new(j_org)
          if(j_new .le. 0) cycle
!
          do kr = r_itp%kr_inner_domain, r_itp%kr_outer_domain
            kr_in =  r_itp%k_old2new_in(kr)
            kr_out = r_itp%k_old2new_out(kr)
            inod_in =  j_org + (kr_in -  1) * org_sph%sph_rj%nidx_rj(2)
            inod_out = j_org + (kr_out - 1) * org_sph%sph_rj%nidx_rj(2)
            inod_new = j_new + (kr - 1) * new_sph%sph_rj%nidx_rj(2)
            d_rj_new(inod_new,nd)                                       &
     &           = r_itp%coef_old2new_in(kr) * d_rj_org(inod_in,nd)     &
     &            + (1.0d0 - r_itp%coef_old2new_in(kr))                 &
     &             * d_rj_org(inod_out,nd)
          end do
        end do
!$omp end do
      end do
!$omp end parallel
!
      end subroutine r_itp_field_data_sph_assemble
!
! -----------------------------------------------------------------------
!
      subroutine mul_sph_magne(b_sph_ratio, nnod_rj,                    &
     &          num_phys_rj, ntot_phys_rj, istack_phys_comp_rj,         &
     &          phys_name_rj, d_rj)
!
      use m_phys_labels
!
      real(kind = kreal), intent(in) :: b_sph_ratio
      integer(kind = kint), intent(in) :: nnod_rj
      integer(kind = kint), intent(in) :: num_phys_rj, ntot_phys_rj
      integer(kind = kint), intent(in)                                  &
     &      :: istack_phys_comp_rj(0:num_phys_rj)
      character(len=kchara) :: phys_name_rj(num_phys_rj)
      real(kind = kreal), intent(inout) :: d_rj(nnod_rj, ntot_phys_rj)
!
      integer(kind = kint) :: nd, i, ist, ied, inod
!
!
      do i = 1, num_phys_rj
        if(    phys_name_rj(i) .eq. fhd_magne                           &
     &    .or. phys_name_rj(i) .eq. fhd_mag_potential                   &
     &    .or. phys_name_rj(i) .eq. fhd_pre_uxb) then
          ist = istack_phys_comp_rj(i-1)
          ied = istack_phys_comp_rj(i  )
          do nd = ist+1, ied
            do inod = 1, nnod_rj
              d_rj(inod,nd) = b_sph_ratio * d_rj(inod,nd)
            end do
          end do
        end if
      end do
!
      end subroutine mul_sph_magne
!
! -------------------------------------------------------------------
!
      end module parallel_assemble_sph
