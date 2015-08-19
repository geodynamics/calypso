!>@file  set_nodal_field_for_psf.f90
!!       module set_nodal_field_for_psf
!!
!!@author H. Matsui
!!@date   Programmed in June, 2006
!
!>@brief Set nodal field for sectioning
!!
!!@verbatim
!!      subroutine count_position_4_psf(psf_list, psf_node)
!!        type(sectioning_list), intent(in) :: psf_list
!!        type(node_data), intent(inout) :: psf_node
!!
!!      subroutine set_position_4_psf(nnod, nedge, nnod_edge, ie_edge,  &
!!     &          xx, istack_intnod, nnod_patch, inod_sum, xx_patch,    &
!!     &          psf_list)
!!
!!      subroutine set_field_on_psf_xyz                                 &
!!     &          (nnod, nedge, nnod_edge, ie_edge, nnod_patch,         &
!!     &          num_fld, ntot_comp, istack_comp_nod,                  &
!!     &          d_nod, ifield_psf, ncomp, dat_tmp, psf_list)
!!
!!      subroutine set_const_on_psf(nnod_patch, const, dat_psf)
!!@endverbatim
!
      module set_nodal_field_for_psf
!
      use m_precision
      use m_constants
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
      subroutine count_position_4_psf(psf_list, psf_node)
!
      use t_psf_geometry_list
      use t_geometry_data
      use cal_minmax_and_stacks
!
      type(sectioning_list), intent(in) :: psf_list
      type(node_data), intent(inout) :: psf_node
!
      integer(kind = kint) :: max_4_smp
!
!
      psf_node%internal_node =   psf_list%internod_on_nod               &
     &                         + psf_list%internod_on_edge
      psf_node%numnod =          psf_node%internal_node                 &
     &                         + psf_list%externod_on_edge              &
     &                         + psf_list%externod_on_nod
      call count_number_4_smp(np_smp, ione, psf_node%internal_node,     &
     &    psf_node%istack_internal_smp, max_4_smp)
      call count_number_4_smp(np_smp, ione, psf_node%numnod,            &
     &    psf_node%istack_nod_smp, max_4_smp)
!
      end subroutine count_position_4_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_position_4_psf(nnod, nedge, nnod_edge, ie_edge,    &
     &          xx, istack_intnod, nnod_patch, inod_sum, xx_patch,      &
     &          psf_list)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nnod, nedge, nnod_edge
      integer(kind = kint), intent(in) :: ie_edge(nedge,nnod_edge)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint_gl), intent(in) :: istack_intnod
      integer(kind = kint), intent(in) :: nnod_patch
      type(sectioning_list), intent(in) :: psf_list
!
      integer(kind = kint_gl), intent(inout) :: inod_sum(nnod_patch)
      real(kind = kreal), intent(inout) :: xx_patch(nnod_patch,3)
!
      integer(kind = kint) :: ishift
!
!
      call set_position_at_nod_psf(nnod, xx, izero,                     &
     &    psf_list%internod_on_nod, psf_list%istack_inter_n_on_n_smp,   &
     &    psf_list%inod_int_nod, istack_intnod, nnod_patch,             &
     &    inod_sum, xx_patch)
!
      ishift =  psf_list%internod_on_nod
      call set_position_on_edge_psf                                     &
     &   (nnod, nedge, nnod_edge, ie_edge, xx, ishift,                  &
     &    psf_list%internod_on_edge, psf_list%istack_inter_n_on_e_smp,  &
     &    psf_list%iedge_int_nod, psf_list%coef_int_edge,               &
     &    istack_intnod, nnod_patch, inod_sum, xx_patch)
!
      ishift = ishift + psf_list%internod_on_edge
      call set_position_at_nod_psf(nnod, xx, ishift,                    &
     &    psf_list%externod_on_nod, psf_list%istack_exter_n_on_n_smp,   &
     &    psf_list%inod_ext_nod, istack_intnod, nnod_patch,             &
     &    inod_sum, xx_patch)
!
      ishift = ishift + psf_list%externod_on_nod
      call set_position_on_edge_psf                                     &
     &   (nnod, nedge, nnod_edge, ie_edge, xx, ishift,                  &
     &    psf_list%externod_on_edge, psf_list%istack_exter_n_on_e_smp,  &
     &    psf_list%iedge_ext_nod, psf_list%coef_ext_edge,               &
     &    istack_intnod, nnod_patch, inod_sum, xx_patch)
!
      end subroutine set_position_4_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_position_at_nod_psf(nnod, xx,                      &
     &          ishift, nnod_on_nod, istack_n_on_n_smp, inod_4_nod,     &
     &          istack_intnod, nnod_patch, inod_sum, xx_patch)
!
      integer(kind = kint), intent(in) :: nnod
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint), intent(in) :: nnod_on_nod, ishift
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
      integer(kind = kint), intent(in) :: inod_4_nod(nnod_on_nod)
      integer(kind = kint_gl), intent(in) :: istack_intnod
      integer(kind = kint), intent(in) :: nnod_patch
!
      integer(kind = kint_gl), intent(inout) :: inod_sum(nnod_patch)
      real(kind = kreal), intent(inout) :: xx_patch(nnod_patch,3)
!
      integer(kind = kint) :: ip, ist, ied, inum
!
!
      call set_field_at_nod_psf(nnod, ithree, xx,                       &
     &   ishift, nnod_on_nod, istack_n_on_n_smp, inod_4_nod,            &
     &   nnod_patch, xx_patch)
!
!$omp parallel do private(ist,ied,inum)
      do ip = 1, np_smp
        ist = istack_n_on_n_smp(ip-1) + 1 + ishift
        ied = istack_n_on_n_smp(ip  ) + ishift
        do inum = ist, ied
          inod_sum(inum) = inum + istack_intnod
        end do
      end do
!$omp end parallel do
!
      end subroutine set_position_at_nod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_position_on_edge_psf                               &
     &         (nnod, nedge, nnod_edge, ie_edge, xx,                    &
     &          ishift, nnod_on_edge, istack_n_on_e_smp, iedge_4_nod,   &
     &          coef_on_edge, istack_intnod, nnod_patch, inod_sum,      &
     &          xx_patch)
!
      integer(kind = kint), intent(in) :: nnod, nedge, nnod_edge
      integer(kind = kint), intent(in) :: ie_edge(nedge,nnod_edge)
      real(kind = kreal), intent(in) :: xx(nnod,3)
!
      integer(kind = kint), intent(in) :: nnod_on_edge, ishift
      integer(kind = kint), intent(in) :: istack_n_on_e_smp(0:np_smp)
      integer(kind = kint), intent(in) :: iedge_4_nod(nnod_on_edge)
      real(kind= kreal), intent(in) :: coef_on_edge(nnod_on_edge,2)
!
      integer(kind = kint_gl), intent(in) :: istack_intnod
      integer(kind = kint), intent(in) :: nnod_patch
!
      integer(kind = kint_gl), intent(inout) :: inod_sum(nnod_patch)
      real(kind = kreal), intent(inout) :: xx_patch(nnod_patch,3)
!
      integer(kind = kint) :: ip, ist, ied, inum
!
!
      call set_field_on_edge_psf                                        &
     &   (nnod, nedge, nnod_edge, ie_edge, ithree, xx,                  &
     &    ishift, nnod_on_edge, istack_n_on_e_smp, iedge_4_nod,         &
     &    coef_on_edge, nnod_patch, xx_patch)
!
!$omp parallel do private(ist,ied,inum)
      do ip = 1, np_smp
        ist = istack_n_on_e_smp(ip-1) + 1 + ishift
        ied = istack_n_on_e_smp(ip  ) + ishift
        do inum = ist, ied
          inod_sum(inum) = inum + istack_intnod
        end do
      end do
!$omp end parallel do
!
      end subroutine set_position_on_edge_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_at_nod_psf(nnod, ncomp, d_nod,               &
     &          ishift, nnod_on_nod, istack_n_on_n_smp, inod_4_nod,     &
     &          nnod_patch, dat_tmp)
!
      integer(kind = kint), intent(in) :: nnod
      integer(kind = kint), intent(in) :: nnod_on_nod, ishift
      integer(kind = kint), intent(in) :: istack_n_on_n_smp(0:np_smp)
      integer(kind = kint), intent(in) :: inod_4_nod(nnod_on_nod)
      integer(kind = kint), intent(in) :: nnod_patch, ncomp
      real(kind = kreal), intent(in) :: d_nod(nnod,ncomp)
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, inum, inod, jnum, nd
!
!
!$omp parallel do private(ist,ied,inum,jnum,inod,nd)
      do ip = 1, np_smp
        ist = istack_n_on_n_smp(ip-1) + 1
        ied = istack_n_on_n_smp(ip  )
        do nd = 1, ncomp
          do inum = ist, ied
            jnum = inum + ishift
            inod = inod_4_nod(inum)
            dat_tmp(jnum,nd) = d_nod(inod,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_field_at_nod_psf
!
!  ---------------------------------------------------------------------
!
      subroutine set_field_on_edge_psf                                  &
     &         (nnod, nedge, nnod_edge, ie_edge, ncomp, d_nod,          &
     &          ishift, nnod_on_edge, istack_n_on_e_smp, iedge_4_nod,   &
     &          coef_on_edge, nnod_patch, dat_tmp)
!
      integer(kind = kint), intent(in) :: nnod, nedge, nnod_edge
      integer(kind = kint), intent(in) :: ie_edge(nedge,nnod_edge)
!
      integer(kind = kint), intent(in) :: nnod_on_edge, ishift
      integer(kind = kint), intent(in) :: istack_n_on_e_smp(0:np_smp)
      integer(kind = kint), intent(in) :: iedge_4_nod(nnod_on_edge)
      real(kind= kreal), intent(in) :: coef_on_edge(nnod_on_edge,2)
!
      integer(kind = kint), intent(in) :: nnod_patch, ncomp
      real(kind = kreal), intent(in) :: d_nod(nnod,ncomp)
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,ncomp)
!
      integer(kind = kint) :: ip, ist, ied, nd
      integer(kind = kint) :: inum, inod1, inod2, iedge, jnum
!
!
!$omp parallel do                                                       &
!$omp& private(ist,ied,inum,jnum,inod1,inod2,iedge,nd)
      do ip = 1, np_smp
        ist = istack_n_on_e_smp(ip-1) + 1
        ied = istack_n_on_e_smp(ip  )
!
        do nd = 1, ncomp
          do inum = ist, ied
            jnum = inum + ishift
            iedge = abs(iedge_4_nod(inum))
            inod1 = ie_edge(iedge,1)
            inod2 = ie_edge(iedge,2)
            dat_tmp(jnum,nd) =  coef_on_edge(inum,1)*d_nod(inod1,nd)    &
     &                        + coef_on_edge(inum,2)*d_nod(inod2,nd)
          end do
        end do
      end do
!$omp end parallel do
!
      end subroutine set_field_on_edge_psf
!
!  ---------------------------------------------------------------------
!  ---------------------------------------------------------------------
!
      subroutine set_field_on_psf_xyz                                   &
     &          (nnod, nedge, nnod_edge, ie_edge, nnod_patch,           &
     &          num_fld, ntot_comp, istack_comp_nod,                    &
     &          d_nod, ifield_psf, ncomp, dat_tmp, psf_list)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nnod, nedge, nnod_edge
      integer(kind = kint), intent(in) :: ie_edge(nedge,nnod_edge)
!
      integer(kind = kint), intent(in) :: nnod_patch
      type(sectioning_list), intent(in) :: psf_list
!
      integer(kind = kint), intent(in) :: num_fld, ntot_comp
      integer(kind = kint), intent(in) :: istack_comp_nod(0:num_fld)
      real(kind = kreal), intent(in) :: d_nod(nnod,ntot_comp)
!
      integer(kind = kint), intent(in) :: ifield_psf, ncomp
!
      real(kind = kreal), intent(inout) :: dat_tmp(nnod_patch,6)
!
      integer(kind = kint) :: ifld, ishift
!
!
      ifld = istack_comp_nod(ifield_psf-1) + 1
      call set_field_at_nod_psf(nnod, ncomp, d_nod(1,ifld), izero,      &
     &    psf_list%internod_on_nod, psf_list%istack_inter_n_on_n_smp,   &
     &    psf_list%inod_int_nod, nnod_patch, dat_tmp(1,1))
!
      ishift = psf_list%internod_on_nod
      call set_field_on_edge_psf                                        &
     &  (nnod, nedge, nnod_edge, ie_edge, ncomp, d_nod(1,ifld), ishift, &
     &   psf_list%internod_on_edge, psf_list%istack_inter_n_on_e_smp,   &
     &   psf_list%iedge_int_nod, psf_list%coef_int_edge,                &
     &   nnod_patch, dat_tmp(1,1))
!
      ishift = ishift + psf_list%internod_on_edge
      call set_field_at_nod_psf(nnod, ncomp, d_nod(1,ifld), ishift,     &
     &    psf_list%externod_on_nod, psf_list%istack_exter_n_on_n_smp,   &
     &    psf_list%inod_ext_nod, nnod_patch, dat_tmp(1,1))
!
      ishift = ishift + psf_list%externod_on_nod
      call set_field_on_edge_psf                                        &
     &  (nnod, nedge, nnod_edge, ie_edge, ncomp, d_nod(1,ifld), ishift, &
     &   psf_list%externod_on_edge, psf_list%istack_exter_n_on_e_smp,   &
     &   psf_list%iedge_ext_nod, psf_list%coef_ext_edge,                &
     &   nnod_patch, dat_tmp(1,1))
!
      end subroutine set_field_on_psf_xyz
!
!  ---------------------------------------------------------------------
!
      subroutine set_const_on_psf(nnod_patch, const, dat_psf)
!
      use t_psf_geometry_list
!
      integer(kind = kint), intent(in) :: nnod_patch
      real(kind = kreal), intent(in) :: const
!
      real(kind = kreal), intent(inout) :: dat_psf(nnod_patch)
!
!
!$omp parallel workshare
          dat_psf(1:nnod_patch) = const
!$omp end parallel workshare
!
      end subroutine set_const_on_psf
!
!  ---------------------------------------------------------------------
!
      end module set_nodal_field_for_psf
