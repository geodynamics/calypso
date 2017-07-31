!>@file   t_sph_mesh_1d_connect.F90
!!@brief  module t_sph_mesh_1d_connect
!!
!!@author H. Matsui
!!@date Programmed in March, 2012
!
!>@brief  One-dimmentional connectivity list for spherical shell
!!
!!@verbatim
!!      subroutine alloc_nnod_nele_sph_mesh(ndomain_sph, ndomain_rtp,&
!!     &          nidx_global_rtp, m_folding, stbl)
!!      subroutine alloc_iele_sph_mesh(stbl)
!!      subroutine alloc_1d_comm_tbl_4_sph                              &
!!     &         (ntot_import, ntot_export, stbl)
!!      subroutine dealloc_nnod_nele_sph_mesh(stbl)
!!      subroutine dealloc_1d_comm_tbl_4_sph(stbl)
!!
!!      subroutine check_iele_4_sph_connects(stbl)
!!@endverbatim
!
      module t_sph_mesh_1d_connect
!
      use m_precision
      use m_constants
!
      implicit none
!
      type comm_table_make_sph
        integer(kind = kint) :: ntot_domain
        integer(kind = kint) :: ndomain_fem(3)
        integer(kind = kint) :: nidx_global_fem(3)
        integer(kind = kint) :: nidx_local_fem(3)
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
      end type comm_table_make_sph
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine alloc_nnod_nele_sph_mesh(ndomain_sph, ndomain_rtp,     &
     &          nidx_global_rtp, m_folding, stbl)
!
      integer(kind = kint), intent(in) :: ndomain_sph
      integer(kind = kint), intent(in) :: ndomain_rtp(3)
      integer(kind = kint), intent(in) :: nidx_global_rtp(3)
      integer(kind = kint), intent(in) :: m_folding
!
      type(comm_table_make_sph), intent(inout) :: stbl
!
      integer(kind = kint) :: np, num
!
!
      stbl%ntot_domain =      ndomain_sph
      stbl%ndomain_fem(1:3) = ndomain_rtp(1:3)
      stbl%nidx_global_fem(1:3) = nidx_global_rtp(1:3)
      stbl%nidx_global_fem(3) =  m_folding * stbl%nidx_global_fem(3)
!
      np =  stbl%ndomain_fem(1)
      num = stbl%nidx_global_fem(1)
      allocate( stbl%iflag_neib_r(np,np) )
      allocate( stbl%iflag_ele_r(num-1,np) )
      allocate( stbl%iflag_internal_r(num,np) )
      allocate( stbl%nnod_sph_r(np) )
      allocate( stbl%nele_sph_r(np) )
      stbl%iflag_neib_r =   0
      stbl%iflag_internal_r = 0
      stbl%iflag_ele_r = 0
      stbl%nnod_sph_r = 0
      stbl%nele_sph_r = 0
!
      allocate( stbl%iflag_center_r(np) )
      allocate( stbl%iflag_ele_center(np) )
      stbl%iflag_center_r =   0
      stbl%iflag_ele_center = 0
!
      np =  stbl%ndomain_fem(2)
      num = stbl%nidx_global_fem(2)
      allocate( stbl%iflag_neib_t(np,np) )
      allocate( stbl%iflag_internal_t(num,np) )
      allocate( stbl%iflag_ele_t(num-1,np) )
      allocate( stbl%nnod_sph_t(np) )
      allocate( stbl%nele_sph_t(np) )
      stbl%iflag_neib_t =   0
      stbl%iflag_internal_t = 0
      stbl%iflag_ele_t = 0
      stbl%nnod_sph_t = 0
      stbl%nele_sph_t = 0
!
      allocate( stbl%iflag_Spole_t(np) )
      allocate( stbl%iflag_Npole_t(np) )
      allocate( stbl%iflag_ele_Spole(np) )
      allocate( stbl%iflag_ele_Npole(np) )
      stbl%iflag_Spole_t = 0
      stbl%iflag_Npole_t = 0
      stbl%iflag_ele_Spole = 0
      stbl%iflag_ele_Npole = 0
!
      allocate( stbl%iflag_center_t(0:num+1) )
      stbl%iflag_center_t = 0
!
      stbl%nele_around_pole = stbl%nidx_global_fem(3) / 2
!
      end subroutine alloc_nnod_nele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine alloc_iele_sph_mesh(stbl)
!
      type(comm_table_make_sph), intent(inout) :: stbl
!
      integer(kind = kint) :: num, np
!
!
      np =  stbl%ndomain_fem(1)
      num = stbl%nidx_global_fem(1)
      allocate( stbl%irev_sph_r(0:num,np) )
      allocate( stbl%inod_sph_r(0:stbl%nmax_nod_sph_r,np) )
      allocate( stbl%ie_sph_r(stbl%nmax_ele_sph_r,2,np) )
      stbl%irev_sph_r = 0
      stbl%inod_sph_r = 0
      stbl%ie_sph_r =   0
!
      allocate( stbl%ie_center_r(2,np) )
      stbl%ie_center_r = 0
!
      np =  stbl%ndomain_fem(2)
      num = stbl%nidx_global_fem(2)
      allocate( stbl%irev_sph_t(0:num+1,np) )
      allocate( stbl%inod_sph_t(0:stbl%nmax_nod_sph_t+1,np) )
      allocate( stbl%ie_sph_t(stbl%nmax_ele_sph_t,2,np) )
      allocate( stbl%ie_center_t(num-1,2) )
      stbl%irev_sph_t = 0
      stbl%inod_sph_t = 0
      stbl%ie_sph_t =    0
      stbl%ie_center_t = 0
      stbl%ie_center_Sp = 0
      stbl%ie_center_Np = 0
!
      allocate( stbl%ie_Spole_t(2,np) )
      allocate( stbl%ie_Npole_t(2,np) )
      stbl%ie_Spole_t =  0
      stbl%ie_Npole_t =  0
!
      allocate( stbl%inod_sph_ct(0:num+1) )
      allocate( stbl%irev_sph_ct(0:num+1) )
      stbl%inod_sph_ct = 0
      stbl%irev_sph_ct = 0
!
      np =  stbl%ndomain_fem(3)
      num = stbl%nidx_global_fem(3)
      allocate( stbl%ie_sph_p(num,2) )
      stbl%ie_sph_p = 0
!!
      end subroutine alloc_iele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine alloc_1d_comm_tbl_4_sph                                &
     &         (ntot_import, ntot_export, stbl)
!
      integer(kind = kint), intent(in) :: ntot_import, ntot_export
      type(comm_table_make_sph), intent(inout) :: stbl
!
!
      allocate(stbl%item_import_rtp(ntot_import))
      allocate(stbl%item_export_rtp(ntot_export))
      allocate(stbl%item_import_1d_rtp(3,ntot_import))
      allocate(stbl%item_export_1d_rtp(3,ntot_export))
      if(ntot_import .gt. 0) stbl%item_import_rtp = 0
      if(ntot_export .gt. 0) stbl%item_export_rtp = 0
      if(ntot_import .gt. 0) stbl%item_import_1d_rtp = 0
      if(ntot_export .gt. 0) stbl%item_export_1d_rtp = 0
!
      end subroutine alloc_1d_comm_tbl_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine dealloc_nnod_nele_sph_mesh(stbl)
!
      type(comm_table_make_sph), intent(inout) :: stbl
!
!
      deallocate(stbl%inod_sph_ct, stbl%irev_sph_ct)
      deallocate(stbl%ie_sph_r, stbl%ie_sph_t, stbl%ie_sph_p)
      deallocate(stbl%irev_sph_r, stbl%irev_sph_t)
      deallocate(stbl%inod_sph_r, stbl%inod_sph_t)
      deallocate(stbl%iflag_internal_r)
      deallocate(stbl%nnod_sph_r, stbl%nele_sph_r)
      deallocate(stbl%iflag_internal_t)
      deallocate(stbl%nnod_sph_t, stbl%nele_sph_t)
      deallocate(stbl%iflag_neib_r, stbl%iflag_neib_t)
      deallocate(stbl%iflag_center_r, stbl%iflag_center_t)
      deallocate(stbl%iflag_Spole_t, stbl%iflag_Npole_t)
      deallocate(stbl%iflag_ele_center)
      deallocate(stbl%iflag_ele_Spole, stbl%iflag_ele_Npole)
      deallocate(stbl%iflag_ele_r, stbl%iflag_ele_t)
      deallocate(stbl%ie_center_t)
!
      end subroutine dealloc_nnod_nele_sph_mesh
!
! ----------------------------------------------------------------------
!
      subroutine dealloc_1d_comm_tbl_4_sph(stbl)
!
      type(comm_table_make_sph), intent(inout) :: stbl
!
!
      deallocate(stbl%item_import_rtp, stbl%item_export_rtp)
      deallocate(stbl%item_import_1d_rtp, stbl%item_export_1d_rtp)
!
      end subroutine dealloc_1d_comm_tbl_4_sph
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_iele_4_sph_connects(stbl)
!
      integer(kind = kint) :: k, ip, i12(2)
      type(comm_table_make_sph), intent(in) :: stbl
!
!
      write(*,'(a,255i6)') 'iflag_neib_r', stbl%ndomain_fem(1)
      do k = 1, stbl%ndomain_fem(1)
        write(*,'(255i6)')                                              &
     &      k, stbl%iflag_neib_r(k,1:stbl%ndomain_fem(1))
      end do
      write(*,*) 'iflag_neib_t', stbl%ndomain_fem(2)
      do k = 1, stbl%ndomain_fem(2)
        write(*,'(255i6)')                                              &
     &      k, stbl%iflag_neib_t(k,1:stbl%ndomain_fem(2))
      end do
!
      write(*,'(a,255i6)') 'iflag_internal_r',                          &
     &                    stbl%nidx_global_fem(1), stbl%ndomain_fem(1)
      write(*,'(a,255i6)') 'Center: ',                                  &
     &                    stbl%iflag_center_r(1:stbl%ndomain_fem(1))
      do k = 0, stbl%nidx_global_fem(2)+1
        write(*,'(255i6)') k, stbl%iflag_center_t(k),                   &
     &                    stbl%inod_sph_ct(k), stbl%irev_sph_ct(k)
      end do
      write(*,'(a)') 'connectivity for center element'
      write(*,'(a,255i6)') 'S-pole: ', stbl%ie_center_Sp
      do k = 1, stbl%nidx_global_fem(2)-1
        write(*,'(255i6)') k, stbl%ie_center_t(k,1:2)
      end do
      write(*,'(a,255i6)') 'N-pole: ', stbl%ie_center_Np
!
      write(*,'(a,255i6)') 'radial numbers: ',                          &
     &         stbl%nnod_sph_r(1:stbl%ndomain_fem(1))
      do k = 1, stbl%nidx_global_fem(1)
        write(*,'(255i6)') k,                                           &
     &                stbl%iflag_internal_r(k,1:stbl%ndomain_fem(1))
      end do
      write(*,*) 'iflag_internal_t',                                    &
     &          stbl%nidx_global_fem(2), stbl%ndomain_fem(2)
      write(*,*) 'numbers: ', stbl%nnod_sph_t
      write(*,'(a,255i6)') 'S_pole: ',                                  &
     &                    stbl%iflag_Spole_t(1:stbl%ndomain_fem(2))
      do k = 1, stbl%nidx_global_fem(2)
        write(*,'(255i6)') k,                                           &
     &                stbl%iflag_internal_t(k,1:stbl%ndomain_fem(2))
      end do
      write(*,'(a,255i6)') 'N_pole: ',                                  &
     &                    stbl%iflag_Npole_t(1:stbl%ndomain_fem(2))
!
      do ip = 1, stbl%ndomain_fem(1)
        write(*,*) 'k, stbl%ie_sph_r(k,1:2,ip) for ',                   &
     &            ip, stbl%nele_sph_r(ip)
        i12(1:2) = stbl%ie_center_r(1:2,ip)
        write(*,*) 'Center: ' , i12(1:2), stbl%inod_sph_r(i12(1:2),ip)
        do k = 1, stbl%nele_sph_r(ip)
          i12(1:2) = stbl%ie_sph_r(k,1:2,ip)
          write(*,*) k, i12(1:2), stbl%inod_sph_r(i12(1:2),ip)
        end do
      end do
!
      do ip = 1, stbl%ndomain_fem(2)
        write(*,*) 'k, stbl%ie_sph_t(k,1:2,ip) for ', ip
        i12(1:2) = stbl%ie_Spole_t(1:2,ip)
        write(*,*) 'S_pole: ', i12(1:2), stbl%inod_sph_t(i12(1:2),ip)
        do k = 1, stbl%nele_sph_t(ip)
          i12(1:2) = stbl%ie_sph_t(k,1:2,ip)
          write(*,*) k, i12(1:2), stbl%inod_sph_t(i12(1:2),ip)
        end do
        i12(1:2) = stbl%ie_Npole_t(1:2,ip)
        write(*,*) 'N_pole: ', i12(1:2), stbl%inod_sph_t(i12(1:2),ip)
      end do
!
      write(*,*) 'k, stbl%ie_sph_p(k,1:2) for all'
      do k = 1, stbl%nidx_global_fem(3)
        write(*,*) k, stbl%ie_sph_p(k,1:2)
      end do
!
      end subroutine check_iele_4_sph_connects
!
! ----------------------------------------------------------------------
!
      end module t_sph_mesh_1d_connect
