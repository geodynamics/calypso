!>@file  copy_psf_data_to_SR.f90
!!       module copy_psf_data_to_SR
!!
!!@author H. Matsui
!!@date   Programmed in March, 2007
!!@n      Modified by H. Matsui in July, 2013
!
!> @brief Copy recieved sectioning data to output data field
!!
!!@verbatim
!!      subroutine set_real_data_2_send_psf(ntot_psf, ncomp_dat,        &
!!     &          dat_psf, send)
!!      subroutine set_int_data_2_send_psf(ntot_psf, ncomp_dat,         &
!!     &          ie_psf, isend)
!!
!!      subroutine set_recv_2_real_data_psf(i_psf, nprocs, num_psf,     &
!!     &          ntot_output, istack_nod_para, istack_nod_recv,        &
!!     &          ncomp_dat, recv, nnod_ucd, ntot_comp_ucd, d_ucd)
!!      subroutine set_recv_2_int_data_psf(i_psf, nprocs, num_psf,      &
!!     &          ntot_output, istack_nod_para,                         &
!!     &          istack_nod_recv, itri, irecv, int_out)
!!
!!      subroutine set_recv_2_ele_connect_psf(i_psf, nprocs, num_psf,   &
!!     &          ntot_output, istack_nod_para, istack_ele_para,        &
!!     &          istack_ele_recv, itri, irecv, ucd_out)
!!        type(ucd_data), intent(inout) :: ucd_out
!!@endverbatim
!
      module copy_psf_data_to_SR
!
      use m_precision
!
      implicit  none
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine set_real_data_2_send_psf(ntot_psf, ncomp_dat,          &
     &          dat_psf, send)
!
      integer(kind = kint), intent(in) :: ntot_psf, ncomp_dat
      real(kind = kreal), intent(in) :: dat_psf(ntot_psf, ncomp_dat)
!
      real(kind = kreal), intent(inout) :: send(ntot_psf*ncomp_dat)
!
      integer(kind = kint) :: inum, nd, k1
!
!
      if(ntot_psf .le. 0) return
!$omp parallel private(nd)
      do nd = 1, ncomp_dat
!$omp do private(inum,k1)
        do inum = 1, ntot_psf
          k1 = (nd-1)*ntot_psf + inum
          send(k1) = dat_psf(inum,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_real_data_2_send_psf
!
! ----------------------------------------------------------------------
!
      subroutine set_int_data_2_send_psf(ntot_psf, ncomp_dat,           &
     &          ie_psf, isend)
!
      integer(kind = kint), intent(in) :: ntot_psf, ncomp_dat
      integer(kind = kint), intent(in) :: ie_psf(ntot_psf, ncomp_dat)
!
      integer(kind = kint), intent(inout) :: isend(ntot_psf*ncomp_dat)
!
      integer(kind = kint) :: inum, nd, k1
!
!
      if(ntot_psf .le. 0) return
!$omp parallel private(nd)
      do nd = 1, ncomp_dat
!$omp do private(inum,k1)
        do inum = 1, ntot_psf
          k1 = (nd-1)*ntot_psf + inum
          isend(k1) = ie_psf(inum,nd)
        end do
!$omp end do nowait
      end do
!$omp end parallel
!
      end subroutine set_int_data_2_send_psf
!
! ----------------------------------------------------------------------
!
      subroutine set_recv_2_real_data_psf(i_psf, nprocs, num_psf,       &
     &          ntot_output, istack_nod_para, istack_nod_recv,          &
     &          ncomp_dat, recv, nnod_ucd, ntot_comp_ucd, d_ucd)
!
      integer(kind = kint), intent(in) :: i_psf, nprocs, num_psf
      integer(kind = kint), intent(in) :: ntot_comp_ucd
      integer(kind = kint), intent(in) :: ntot_output, ncomp_dat
      integer(kind = kint), intent(in) :: istack_nod_para(0:nprocs)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      integer(kind = kint_gl), intent(in) :: nnod_ucd
      real(kind = kreal), intent(in) :: recv(ntot_output*ncomp_dat)
!
      real(kind = kreal), intent(inout)                                 &
     &                   :: d_ucd(nnod_ucd,ntot_comp_ucd)
!
      integer(kind = kint) :: inum, nd, ip, k0, kk
      integer(kind = kint) :: inod, jnod, kst, num
!
!
      do nd = 1, ntot_comp_ucd
        do ip = 1, nprocs
          k0 = (ip-1)*num_psf
          kst = ncomp_dat * istack_nod_recv(k0)                         &
     &         + (nd-1) * (istack_nod_recv(num_psf+k0)                  &
     &                    -istack_nod_recv( k0 ))                       &
     &         + (istack_nod_recv(k0+i_psf-1) - istack_nod_recv(k0))
          num = istack_nod_para(ip) - istack_nod_para(ip-1)
          do inum = 1, num
            jnod = inum + istack_nod_para(ip-1)
            inod = inum + istack_nod_para(ip-1) - istack_nod_para(0)
            kk = inum + kst
            d_ucd(inod,nd) =   recv(kk)
          end do
        end do
      end do
!
      end subroutine set_recv_2_real_data_psf
!
! ----------------------------------------------------------------------
!
      subroutine set_recv_2_int_data_psf(i_psf, nprocs, num_psf,        &
     &          ntot_output, istack_nod_para,                           &
     &          istack_nod_recv, itri, irecv, int_out)
!
      integer(kind = kint), intent(in) :: i_psf, nprocs, num_psf
      integer(kind = kint), intent(in) :: ntot_output, itri
      integer(kind = kint), intent(in) :: istack_nod_para(0:nprocs)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_nod_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: irecv(ntot_output*itri)
!
      integer(kind = kint), intent(inout) :: int_out(ntot_output,itri)
!
      integer(kind = kint) :: inum, nd, ip, k0, kk, jele, kst, num
!
!
      do nd = 1, itri
        do ip = 1, nprocs
            k0 = (ip-1)*num_psf
            kst = itri * istack_nod_recv(k0)                            &
     &           + (nd-1) * (istack_nod_recv(num_psf+k0)                &
     &                       -istack_nod_recv( k0 ))                    &
     &            + (istack_nod_recv(k0+i_psf-1) - istack_nod_recv(k0))
          num = istack_nod_para(ip) - istack_nod_para(ip-1)
          do inum = 1, num
            jele = istack_nod_para(ip-1) + inum
            kk = kst + inum
            int_out(jele,nd) = irecv(kk)
          end do
        end do
      end do
!
      end subroutine set_recv_2_int_data_psf
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine set_recv_2_ele_connect_psf(i_psf, nprocs, num_psf,     &
     &          ntot_output, istack_nod_para, istack_ele_para,          &
     &          istack_ele_recv, itri, irecv, ucd_out)
!
      use t_ucd_data
!
      integer(kind = kint), intent(in) :: i_psf, num_psf, nprocs
      integer(kind = kint), intent(in) :: ntot_output, itri
      integer(kind = kint), intent(in) :: istack_nod_para(0:nprocs)
      integer(kind = kint), intent(in) :: istack_ele_para(0:nprocs)
      integer(kind = kint), intent(in)                                  &
     &      :: istack_ele_recv(0:nprocs*num_psf)
      integer(kind = kint), intent(in) :: irecv(ntot_output*itri)
!
      type(ucd_data), intent(inout) :: ucd_out
!
      integer(kind = kint) :: inum, k0, kk, kst, num
      integer(kind = kint) :: ip, nd, iele
!
!
      do nd = 1, itri
        do ip = 1, nprocs
          k0 = (ip-1)*num_psf
          kst = itri * istack_ele_recv(k0)                              &
     &         + (nd-1) * (istack_ele_recv(num_psf+k0)                  &
     &                    -istack_ele_recv( k0 ))                       &
     &          + (istack_ele_recv(k0+i_psf-1) - istack_ele_recv(k0))
          num = istack_ele_para(ip) - istack_ele_para(ip-1)
          do inum = 1, num
            iele = inum + istack_ele_para(ip-1) - istack_ele_para(0)
            kk = kst + inum
!
            ucd_out%ie(iele,nd) = irecv(kk) + istack_nod_para(ip-1)         &
     &                                  - istack_nod_para(0)
          end do
        end do
      end do
!
      end subroutine set_recv_2_ele_connect_psf
!
! ----------------------------------------------------------------------
!
      end module copy_psf_data_to_SR
