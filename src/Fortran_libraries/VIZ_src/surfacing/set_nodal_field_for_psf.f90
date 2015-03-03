!set_nodal_field_for_psf.f90
!      module set_nodal_field_for_psf
!
!      Written by H. Matsui on June, 2006
!
!      subroutine count_position_4_psf(istack_n_smp,                    &
!     &          istack_n_on_n_smp, istack_n_on_e_smp)
!
!      subroutine set_position_4_psf(nnod, nedge, nnod_edge, ie_edge,   &
!     &          inod_global, xx, nnod_patch, istack_n_smp,             &
!     &          nnod_on_nod, istack_n_on_n_smp, nnod_on_edge,          &
!     &          istack_n_on_e_smp, inod_4_nod, iedge_4_nod,            &
!     &          coef_on_edge, inod_sum, xx_patch)
!
!      subroutine set_field_on_psf_xyz(nnod, nedge, nnod_edge,          &
!     &          ie_edge, nnod_patch, istack_n_smp, nnod_on_nod,        &
!     &          istack_n_on_n_smp, nnod_on_edge,                       &
!     &          istack_n_on_e_smp, inod_4_nod, iedge_4_nod,            &
!     &          coef_on_edge, num_fld, ntot_comp, istack_comp_nod,     &
!     &          d_nod, ifield_psf, ncomp_org, dat_tmp)
!
!!      subroutine set_const_on_psf(nnod_patch, istack_n_smp,           &
!!     &          const, dat_psf, psf_list)
!
      module set_nodal_field_for_psf
!
      use m_precision
!
      use m_machine_parameter
!
      implicit none
!
!  ---------------------------------------------------------------------
!
      contains
!
!  ---------------------------------------------------------------------
!
      subroutine count_position_4_psf(istack_n_smp,                     &
     &          istack_n_on_n_smp, istack_n_on_e_smp)
!
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
      integer(kind = kint), intent(in) :: istack_n_on_e_smp(0:np_smp)
!
      integer(kind = kint), intent(inout) :: istack_n_smp(0:np_smp)
!
      integer(kind = kint) :: ip
!
!
      do ip = 1, np_smp
        istack_n_smp(ip) = istack_n_smp(ip-1)                           &
     &                    + istack_n_on_n_smp(ip)                       &
     &                    - istack_n_on_n_smp(ip-1)                     &
     &                    + istack_n_on_e_smp(ip)                       &
     &                    - istack_n_on_e_smp(ip-1)
      end do
!
      end subroutine count_position_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_position_4_psf(nnod, nedge, nnod_edge, ie_edge,    &
     &          inod_global, xx, nnod_patch, istack_n_smp,              &
     &          inod_sum, xx_patch, psf_list)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nnod, nedge, nnod_edge
      integer(kind = kint_gl), intent(in) :: inod_global(nnod)
      integer(kind = kint), intent(in) :: ie_edge(nedge,nnod_edge)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint), intent(in) :: nnod_patch
      integer(kind = kint), intent(in) :: istack_n_smp(0:np_smp)
      type(sectiong_list), intent(in) :: psf_list
!
      integer(kind = kint), intent(inout) :: inod_sum(nnod_patch)
      real(kind = kreal), intent(inout) :: xx_patch(nnod_patch,3)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: ist, num_n, num_e
      integer(kind = kint) :: i, inum, inod, inod1, inod2, iedge, jnum
!
!
!$omp parallel do                                                       &
!$omp& private(ist,num_n,num_e,i,inum,jnum,inod,inod1,inod2,iedge)
      do ip = 1, np_smp
        ist =   istack_n_smp(ip-1)
        num_n = psf_list%istack_n_on_n_smp(ip)                          &
     &         - psf_list%istack_n_on_n_smp(ip-1)
        num_e = psf_list%istack_n_on_e_smp(ip)                          &
     &         - psf_list%istack_n_on_e_smp(ip-1)
!
        do i = 1, num_n
          jnum = ist + i
          inum = i + psf_list%istack_n_on_n_smp(ip-1)
          inod = psf_list%inod_4_nod(inum)
          xx_patch(jnum,1) = xx(inod,1)
          xx_patch(jnum,2) = xx(inod,2)
          xx_patch(jnum,3) = xx(inod,3)
          inod_sum(jnum) =   int(inod_global(inod))
        end do
!
        do i = 1, num_e
          jnum = ist + num_n + i
          inum = i + psf_list%istack_n_on_e_smp(ip-1)
          iedge = abs(psf_list%iedge_4_nod(inum))
          inod1 = ie_edge(iedge,1)
          inod2 = ie_edge(iedge,2)
          xx_patch(jnum,1) =  psf_list%coef_on_edge(inum,1)*xx(inod1,1) &
     &                      + psf_list%coef_on_edge(inum,2)*xx(inod2,1)
          xx_patch(jnum,2) =  psf_list%coef_on_edge(inum,1)*xx(inod1,2) &
     &                      + psf_list%coef_on_edge(inum,2)*xx(inod2,2)
          xx_patch(jnum,3) =  psf_list%coef_on_edge(inum,1)*xx(inod1,3) &
     &                      + psf_list%coef_on_edge(inum,2)*xx(inod2,3)
          inod_sum(jnum) = int(inod_global(inod1) + inod_global(inod2))
        end do
!
      end do
!$omp end parallel do
!
      end subroutine set_position_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_on_psf_xyz(nnod, nedge, nnod_edge,           &
     &          ie_edge, nnod_patch, istack_n_smp,                      &
     &          num_fld, ntot_comp, istack_comp_nod,                    &
     &          d_nod, ifield_psf, ncomp_org, dat_tmp, psf_list)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nnod, nedge, nnod_edge
      integer(kind = kint), intent(in) :: ie_edge(nedge,nnod_edge)
!
      integer(kind = kint), intent(in) :: nnod_patch
      integer(kind = kint), intent(in) :: istack_n_smp(0:np_smp)
      type(sectiong_list), intent(in) :: psf_list
!
      integer(kind = kint), intent(in) :: num_fld, ntot_comp
      integer(kind = kint), intent(in) :: istack_comp_nod(0:num_fld)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) :: ifield_psf, ncomp_org
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,6)
!
      integer(kind = kint) :: ip, nd
      integer(kind = kint) :: icomp
      integer(kind = kint) :: ist, num_n, num_e
      integer(kind = kint) :: i, inum, inod, inod1, inod2, iedge, jnum
!

!$omp parallel do private(nd,icomp,ist,num_n,num_e,i,                   &
!$omp&                    inum,inod,inod1,inod2,iedge,jnum)
        do ip = 1, np_smp
          ist =   istack_n_smp(ip-1)
          num_n = psf_list%istack_n_on_n_smp(ip)                        &
     &           - psf_list%istack_n_on_n_smp(ip-1)
          num_e = psf_list%istack_n_on_e_smp(ip)                        &
     &           - psf_list%istack_n_on_e_smp(ip-1)
!
          do nd = 1, ncomp_org
            icomp = istack_comp_nod(ifield_psf-1) + nd
!
            do i = 1, num_n
              jnum = ist + i
              inum = i + psf_list%istack_n_on_n_smp(ip-1)
              inod = psf_list%inod_4_nod(inum)
              dat_tmp(jnum,nd) = d_nod(inod,icomp)
            end do
!
            do i = 1, num_e
              jnum = ist + num_n + i
              inum = i + psf_list%istack_n_on_e_smp(ip-1)
              iedge = abs(psf_list%iedge_4_nod(inum))
              inod1 = ie_edge(iedge,1)
              inod2 = ie_edge(iedge,2)
              dat_tmp(jnum,nd)                                          &
     &           = psf_list%coef_on_edge(inum,1)*d_nod(inod1,icomp)     &
     &           + psf_list%coef_on_edge(inum,2)*d_nod(inod2,icomp)
            end do
!
          end do
        end do
!$omp end parallel do
!
      end subroutine set_field_on_psf_xyz
!
!  ---------------------------------------------------------------------
!
      subroutine set_const_on_psf(nnod_patch, istack_n_smp,             &
     &          const, dat_psf, psf_list)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nnod_patch
      integer(kind = kint), intent(in) :: istack_n_smp(0:np_smp)
      real(kind = kreal), intent(in) :: const
      type(sectiong_list), intent(in) :: psf_list
!
      real(kind = kreal), intent(inout) :: dat_psf(nnod_patch)
!
      integer(kind = kint) :: ip
      integer(kind = kint) :: ist, num_n, num_e
      integer(kind = kint) :: i, inum
!
!
!$omp parallel do private(i,inum,ist,num_n,num_e)
      do ip = 1, np_smp
        ist =   istack_n_smp(ip-1)
        num_n = psf_list%istack_n_on_n_smp(ip)                          &
     &          - psf_list%istack_n_on_n_smp(ip-1)
        num_e = psf_list%istack_n_on_e_smp(ip)                          &
     &          - psf_list%istack_n_on_e_smp(ip-1)
!
        do i = 1, num_n
          inum  = ist + i
          dat_psf(inum) = const
        end do
!
        do i = 1, num_e
          inum  = ist + num_n + i
          dat_psf(inum) = const
        end do
      end do
!$omp end parallel do
!
      end subroutine set_const_on_psf
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_field_for_psf
