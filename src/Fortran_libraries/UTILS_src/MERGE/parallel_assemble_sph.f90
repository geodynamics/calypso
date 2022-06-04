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
!!      subroutine copy_field_data_sph_assemble                         &
!!     &         (org_sph, new_sph, j_table, org_fld, new_fld)
!!      subroutine copy_field_data_sph_center                           &
!!     &         (org_sph, new_sph, j_table, org_fld, new_fld)
!!      subroutine r_itp_field_data_sph_assemble                        &
!!     &         (org_sph, new_sph, r_itp, j_table, org_fld, new_fld)
!!        type(sph_grids), intent(in) :: org_sph
!!        type(sph_grids), intent(in) :: new_sph
!!        type(rj_assemble_tbl), intent(in) :: j_table
!!        type(sph_radial_interpolate), intent(in) :: r_itp
!!        type(phys_data), intent(in) :: org_fld
!!        type(phys_data), intent(inout) :: new_fld
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
      use t_spheric_parameter
      use t_phys_data
!
      implicit none
!
      type rj_assemble_tbl
        integer(kind = kint), allocatable :: j_org_to_new(:)
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
      use t_spheric_rj_data
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
        j_new = find_local_sph_address(new_sph%sph_rj, l_gl, m_gl)
        if(j_new .gt. 0) then
          j_table%j_org_to_new(j_org) = j_new
        else
          j_table%j_org_to_new(j_org) = 0
        end if
      end do
!
      if(new_sph%sph_rj%inod_rj_center .gt. 0                           &
      &   .and. org_sph%sph_rj%inod_rj_center .gt. 0) then
        j_table%icenter = new_sph%sph_rj%inod_rj_center
      else
        j_table%icenter = 0
      end if
!
      end subroutine set_mode_table_4_assemble
!
! -------------------------------------------------------------------
!
      subroutine copy_field_data_sph_assemble                           &
     &         (org_sph, new_sph, j_table, org_fld, new_fld)
!
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: org_sph
      type(sph_grids), intent(in) :: new_sph
      type(rj_assemble_tbl), intent(in) :: j_table
      type(phys_data), intent(in) :: org_fld
!
      type(phys_data), intent(inout) :: new_fld
!
!
      integer(kind = kint) :: nd, j_org, j_new, kr
      integer(kind = kint) :: inod_org, inod_new
!
!!$omp parallel private(nd)
      do nd = 1, new_fld%ntot_phys
!!$omp do private(j_org,j_new,kr,inod_org,inod_new)
        do j_org = 1, org_sph%sph_rj%nidx_rj(2)
          j_new = j_table%j_org_to_new(j_org)
          if(j_new .le. 0) cycle
!
          do kr = 1, org_sph%sph_rj%nidx_rj(1)
            inod_org = j_org + (kr - 1) * org_sph%sph_rj%nidx_rj(2)
            inod_new = j_new + (kr - 1) * new_sph%sph_rj%nidx_rj(2)
            new_fld%d_fld(inod_new,nd) = org_fld%d_fld(inod_org,nd)
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
      subroutine copy_field_data_sph_center                             &
     &         (org_sph, new_sph, j_table, org_fld, new_fld)
!
      use t_spheric_parameter
!
      type(sph_grids), intent(in) :: org_sph
      type(sph_grids), intent(in) :: new_sph
      type(rj_assemble_tbl), intent(in) :: j_table
      type(phys_data), intent(in) :: org_fld
!
      type(phys_data), intent(inout) :: new_fld
!
      integer(kind = kint) :: inod_org, inod_new
      integer(kind = kint) :: i_fld, ist, num, jj
!
!
      if(new_sph%sph_rj%inod_rj_center .eq. 0) return
!
      if(j_table%icenter .gt. 0) then
        inod_org = org_sph%sph_rj%inod_rj_center
        inod_new = new_sph%sph_rj%inod_rj_center
!$omp parallel workshare
        new_fld%d_fld(inod_new,1:new_fld%ntot_phys)                     &
     &          = org_fld%d_fld(inod_org,1:new_fld%ntot_phys)
!$omp end parallel workshare
!
      else if(j_table%icenter .eq. 0) then
        jj = find_local_sph_address(org_sph%sph_rj, 0, 0)
        if(jj .gt. 0) then
          inod_org = local_sph_node_address(org_sph%sph_rj, 1, jj)
          inod_new = new_sph%sph_rj%inod_rj_center
!$omp parallel do private(i_fld,ist,num)
          do i_fld = 1, new_fld%num_phys
            ist = new_fld%istack_component(i_fld-1)
            num = new_fld%istack_component(i_fld) - ist
            if(num .eq. 1) then
              new_fld%d_fld(inod_new,ist+1)                             &
     &            = org_fld%d_fld(inod_org,ist+1)
            else
              new_fld%d_fld(inod_new,ist+1) = 0.0d0
            end if
          end do
!$omp end parallel do
!
        end if
      end if
!
      end subroutine copy_field_data_sph_center
!
! -------------------------------------------------------------------
!
      subroutine r_itp_field_data_sph_assemble                          &
     &         (org_sph, new_sph, r_itp, j_table, org_fld, new_fld)
!
      use t_sph_radial_interpolate
!
      type(sph_grids), intent(in) :: org_sph
      type(sph_grids), intent(in) :: new_sph
      type(rj_assemble_tbl), intent(in) :: j_table
      type(sph_radial_interpolate), intent(in) :: r_itp
      type(phys_data), intent(in) :: org_fld
!
      type(phys_data), intent(inout) :: new_fld
!
      integer(kind = kint) :: nd, j_org, j_new, kr, kr_in, kr_out
      integer(kind = kint) :: inod_in, inod_out, inod_new
!
!$omp parallel private(nd)
      do nd = 1, new_fld%ntot_phys
!$omp do private(j_org,j_new,kr,kr_in,kr_out,inod_in,inod_out,inod_new)
        do j_org = 1, org_sph%sph_rj%nidx_rj(2)
          j_new = j_table%j_org_to_new(j_org)
          if(j_new .le. 0) cycle
!
          do kr = r_itp%kr_inner_source, r_itp%kr_outer_source
            kr_in =  r_itp%k_old2new_in(kr)
            kr_out = r_itp%k_old2new_out(kr)
            inod_in =  j_org + (kr_in -  1) * org_sph%sph_rj%nidx_rj(2)
            inod_out = j_org + (kr_out - 1) * org_sph%sph_rj%nidx_rj(2)
            inod_new = j_new + (kr - 1) * new_sph%sph_rj%nidx_rj(2)
            new_fld%d_fld(inod_new,nd)                                  &
     &         = r_itp%coef_old2new_in(kr) * org_fld%d_fld(inod_in,nd)  &
     &          + (1.0d0 - r_itp%coef_old2new_in(kr))                   &
     &           * org_fld%d_fld(inod_out,nd)
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
      end module parallel_assemble_sph
