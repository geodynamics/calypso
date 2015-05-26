!>@file   m_sph_mesh_1d_connect.F90
!!@brief  module m_sph_mesh_1d_connect
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief  One-dimmentional connectivity list for spherical shell
!!
!!@verbatim
!!      subroutine allocate_radius_1d_gl(nri_global)
!!      subroutine deallocate_radius_1d_gl
!!
!!      subroutine allocate_nnod_nele_sph_mesh(ndomain_sph, ndomain_rtp,&
!!     &          nidx_global_rtp, m_folding)
!!      subroutine allocate_iele_sph_mesh
!!      subroutine deallocate_nnod_nele_sph_mesh
!!
!!      subroutine check_iele_4_sph_connects
!!@endverbatim
!
      module m_sph_mesh_1d_connect
!
      use m_precision
      use m_constants
!
      implicit none
!
      integer(kind = kint) :: ntot_domain
      integer(kind = kint) :: ndomain_fem(3)
      integer(kind = kint) :: nidx_global_fem(3)
      integer(kind = kint) :: nidx_local_fem(3)
!
!>      global radius data @f$ r(k) @f$
      real(kind = kreal), allocatable :: radius_1d_gl(:)
!
      integer(kind = kint), allocatable :: iflag_neib_r(:,:)
      integer(kind = kint), allocatable :: iflag_neib_t(:,:)
!
      integer(kind = kint), allocatable :: item_import_rtp(:)
      integer(kind = kint), allocatable :: item_export_rtp(:)
      integer(kind = kint), allocatable :: item_import_1d_rtp(:,:)
      integer(kind = kint), allocatable :: item_export_1d_rtp(:,:)
!
      integer(kind = kint), allocatable :: iflag_internal_r(:,:)
      integer(kind = kint), allocatable :: iflag_internal_t(:,:)
!
      integer(kind = kint), allocatable :: iflag_Spole_t(:)
      integer(kind = kint), allocatable :: iflag_Npole_t(:)
      integer(kind = kint), allocatable :: iflag_center_r(:)
      integer(kind = kint), allocatable :: iflag_center_t(:)
!
      integer(kind = kint), allocatable :: iflag_ele_r(:,:)
      integer(kind = kint), allocatable :: iflag_ele_t(:,:)
!
      integer(kind = kint), allocatable :: iflag_ele_Spole(:)
      integer(kind = kint), allocatable :: iflag_ele_Npole(:)
      integer(kind = kint), allocatable :: iflag_ele_center(:)
!
      integer(kind = kint), allocatable :: nnod_sph_r(:)
      integer(kind = kint), allocatable :: nnod_sph_t(:)
!
      integer(kind = kint), allocatable :: nele_sph_r(:)
      integer(kind = kint), allocatable :: nele_sph_t(:)
!
      integer(kind = kint) :: nmax_nod_sph_r
      integer(kind = kint) :: nmax_nod_sph_t
!  1D global node address
      integer(kind = kint), allocatable :: inod_sph_r(:,:)
      integer(kind = kint), allocatable :: inod_sph_t(:,:)
!  1D local node address at global node address
      integer(kind = kint), allocatable :: irev_sph_r(:,:)
      integer(kind = kint), allocatable :: irev_sph_t(:,:)
!
      integer(kind = kint) :: nnod_sph_ct
      integer(kind = kint), allocatable :: inod_sph_ct(:)
      integer(kind = kint), allocatable :: irev_sph_ct(:)
!
      integer(kind = kint) :: nmax_ele_sph_r
      integer(kind = kint) :: nmax_ele_sph_t
!
!  1D element address using local 1d node ID
      integer(kind = kint), allocatable :: ie_sph_r(:,:,:)
      integer(kind = kint), allocatable :: ie_sph_t(:,:,:)
      integer(kind = kint), allocatable :: ie_sph_p(:,:)
!
      integer(kind = kint) :: nele_around_pole
      integer(kind = kint), allocatable :: ie_Spole_t(:,:)
      integer(kind = kint), allocatable :: ie_Npole_t(:,:)
      integer(kind = kint), allocatable :: ie_center_r(:,:)
      integer(kind = kint), allocatable :: ie_center_t(:,:)
      integer(kind = kint) :: ie_center_Sp(2)
      integer(kind = kint) :: ie_center_Np(2)
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine allocate_radius_1d_gl(nri_global)
!
      integer(kind = kint), intent(in) :: nri_global
!
!
      allocate(radius_1d_gl(nri_global))
      if(nri_global .gt. 0) radius_1d_gl = 0.0d0
!
      end subroutine allocate_radius_1d_gl
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_radius_1d_gl
!
      deallocate(radius_1d_gl)
!
      end subroutine deallocate_radius_1d_gl
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_nnod_nele_sph_mesh(ndomain_sph, ndomain_rtp,  &
     &          nidx_global_rtp, m_folding)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      integer(kind = kint), intent(in) :: ndomain_rtp(3)
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
      integer(kind = kint), intent(in) :: m_folding
      integer(kind = kint) :: np, num
!
!
      ntot_domain =      ndomain_sph
      ndomain_fem(1:3) = ndomain_rtp(1:3)
      nidx_global_fem(1:3) = nidx_global_rtp(1:3)
      nidx_global_fem(3) =  m_folding * nidx_global_fem(3)
!
      np =  ndomain_fem(1)
      num = nidx_global_fem(1)
      allocate( iflag_neib_r(np,np) )
      allocate( iflag_ele_r(num-1,np) )
      allocate( iflag_internal_r(num,np) )
      allocate( nnod_sph_r(np) )
      allocate( nele_sph_r(np) )
      iflag_neib_r =   0
      iflag_internal_r = 0
      iflag_ele_r = 0
      nnod_sph_r = 0
      nele_sph_r = 0
!
      allocate( iflag_center_r(np) )
      allocate( iflag_ele_center(np) )
      iflag_center_r =   0
      iflag_ele_center = 0
!
      np =  ndomain_fem(2)
      num = nidx_global_fem(2)
      allocate( iflag_neib_t(np,np) )
      allocate( iflag_internal_t(num,np) )
      allocate( iflag_ele_t(num-1,np) )
      allocate( nnod_sph_t(np) )
      allocate( nele_sph_t(np) )
      iflag_neib_t =   0
      iflag_internal_t = 0
      iflag_ele_t = 0
      nnod_sph_t = 0
      nele_sph_t = 0
!
      allocate( iflag_Spole_t(np) )
      allocate( iflag_Npole_t(np) )
      allocate( iflag_ele_Spole(np) )
      allocate( iflag_ele_Npole(np) )
      iflag_Spole_t = 0
      iflag_Npole_t = 0
      iflag_ele_Spole = 0
      iflag_ele_Npole = 0
!
      allocate( iflag_center_t(0:num+1) )
      iflag_center_t = 0
!
      nele_around_pole = nidx_global_fem(3) / 2
!
      end subroutine allocate_nnod_nele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine allocate_iele_sph_mesh
!
      integer(kind = kint) :: num, np
!
!
      np =  ndomain_fem(1)
      num = nidx_global_fem(1)
      allocate( irev_sph_r(0:num,np) )
      allocate( inod_sph_r(0:nmax_nod_sph_r,np) )
      allocate( ie_sph_r(nmax_ele_sph_r,2,np) )
      irev_sph_r = 0
      inod_sph_r = 0
      ie_sph_r =   0
!
      allocate( ie_center_r(2,np) )
      ie_center_r = 0
!
      np =  ndomain_fem(2)
      num = nidx_global_fem(2)
      allocate( irev_sph_t(0:num+1,np) )
      allocate( inod_sph_t(0:nmax_nod_sph_t+1,np) )
      allocate( ie_sph_t(nmax_ele_sph_t,2,np) )
      allocate( ie_center_t(num-1,2) )
      irev_sph_t = 0
      inod_sph_t = 0
      ie_sph_t =    0
      ie_center_t = 0
      ie_center_Sp = 0
      ie_center_Np = 0
!
      allocate( ie_Spole_t(2,np) )
      allocate( ie_Npole_t(2,np) )
      ie_Spole_t =  0
      ie_Npole_t =  0
!
      allocate( inod_sph_ct(0:num+1) )
      allocate( irev_sph_ct(0:num+1) )
      inod_sph_ct = 0
      irev_sph_ct = 0
!
      np =  ndomain_fem(3)
      num = nidx_global_fem(3)
      allocate( ie_sph_p(num,2) )
      ie_sph_p = 0
!!
      end subroutine allocate_iele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine allocate_1d_comm_tbl_4_sph(ntot_import, ntot_export)
!
      integer(kind = kint), intent(in) :: ntot_import, ntot_export
!
!
      allocate(item_import_rtp(ntot_import))
      allocate(item_export_rtp(ntot_export))
      allocate(item_import_1d_rtp(3,ntot_import))
      allocate(item_export_1d_rtp(3,ntot_export))
      if(ntot_import .gt. 0) item_import_rtp = 0
      if(ntot_export .gt. 0) item_export_rtp = 0
      if(ntot_import .gt. 0) item_import_1d_rtp = 0
      if(ntot_export .gt. 0) item_export_1d_rtp = 0
!
      end subroutine allocate_1d_comm_tbl_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine deallocate_nnod_nele_sph_mesh
!
!
      deallocate(inod_sph_ct, irev_sph_ct)
      deallocate(ie_sph_r, ie_sph_t, ie_sph_p)
      deallocate(irev_sph_r, irev_sph_t)
      deallocate(inod_sph_r, inod_sph_t)
      deallocate(iflag_internal_r, nnod_sph_r, nele_sph_r)
      deallocate(iflag_internal_t, nnod_sph_t, nele_sph_t)
      deallocate(iflag_neib_r, iflag_neib_t)
      deallocate(iflag_center_r, iflag_center_t)
      deallocate(iflag_Spole_t, iflag_Npole_t)
      deallocate(iflag_ele_center, iflag_ele_Spole, iflag_ele_Npole)
      deallocate(iflag_ele_r, iflag_ele_t)
      deallocate(ie_center_t)
!
      end subroutine deallocate_nnod_nele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_1d_comm_tbl_4_sph
!
!
      deallocate(item_import_rtp, item_export_rtp)
      deallocate(item_import_1d_rtp, item_export_1d_rtp)
!
      end subroutine deallocate_1d_comm_tbl_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_iele_4_sph_connects
!
      integer(kind = kint) :: k, ip, i12(2)
!
!
      write(*,'(a,255i6)') 'iflag_neib_r', ndomain_fem(1)
      do k = 1, ndomain_fem(1)
        write(*,'(255i6)') k, iflag_neib_r(k,1:ndomain_fem(1))
      end do
      write(*,*) 'iflag_neib_t', ndomain_fem(2)
      do k = 1, ndomain_fem(2)
        write(*,'(255i6)') k, iflag_neib_t(k,1:ndomain_fem(2))
      end do
!
      write(*,'(a,255i6)') 'iflag_internal_r',                          &
     &                       nidx_global_fem(1), ndomain_fem(1)
      write(*,'(a,255i6)') 'Center: ', iflag_center_r(1:ndomain_fem(1))
      do k = 0, nidx_global_fem(2)+1
        write(*,'(255i6)') k, iflag_center_t(k),                        &
     &                    inod_sph_ct(k), irev_sph_ct(k)
      end do
      write(*,'(a)') 'connectivity for center element'
      write(*,'(a,255i6)') 'S-pole: ', ie_center_Sp
      do k = 1, nidx_global_fem(2)-1
        write(*,'(255i6)') k, ie_center_t(k,1:2)
      end do
      write(*,'(a,255i6)') 'N-pole: ', ie_center_Np
!
      write(*,'(a,255i6)') 'radial numbers: ',                          &
     &         nnod_sph_r(1:ndomain_fem(1))
      do k = 1, nidx_global_fem(1)
        write(*,'(255i6)') k, iflag_internal_r(k,1:ndomain_fem(1))
      end do
      write(*,*) 'iflag_internal_t', nidx_global_fem(2), ndomain_fem(2)
      write(*,*) 'numbers: ', nnod_sph_t
      write(*,'(a,255i6)') 'S_pole: ', iflag_Spole_t(1:ndomain_fem(2))
      do k = 1, nidx_global_fem(2)
        write(*,'(255i6)') k, iflag_internal_t(k,1:ndomain_fem(2))
      end do
      write(*,'(a,255i6)') 'N_pole: ', iflag_Npole_t(1:ndomain_fem(2))
!
      do ip = 1, ndomain_fem(1)
        write(*,*) 'k, ie_sph_r(k,1:2,ip) for ', ip, nele_sph_r(ip)
        i12(1:2) = ie_center_r(1:2,ip)
        write(*,*) 'Center: ' , i12(1:2), inod_sph_r(i12(1:2),ip)
        do k = 1, nele_sph_r(ip)
          i12(1:2) = ie_sph_r(k,1:2,ip)
          write(*,*) k, i12(1:2), inod_sph_r(i12(1:2),ip)
        end do
      end do
!
      do ip = 1, ndomain_fem(2)
        write(*,*) 'k, ie_sph_t(k,1:2,ip) for ', ip
        i12(1:2) = ie_Spole_t(1:2,ip)
        write(*,*) 'S_pole: ', i12(1:2), inod_sph_t(i12(1:2),ip)
        do k = 1, nele_sph_t(ip)
          i12(1:2) = ie_sph_t(k,1:2,ip)
          write(*,*) k, i12(1:2), inod_sph_t(i12(1:2),ip)
        end do
        i12(1:2) = ie_Npole_t(1:2,ip)
        write(*,*) 'N_pole: ', i12(1:2), inod_sph_t(i12(1:2),ip)
      end do
!
      write(*,*) 'k, ie_sph_p(k,1:2) for all'
      do k = 1, nidx_global_fem(3)
        write(*,*) k, ie_sph_p(k,1:2)
      end do
!
      end subroutine check_iele_4_sph_connects
!
! ----------------------------------------------------------------------
!
      end module m_sph_mesh_1d_connect
