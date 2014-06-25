!
!      module m_work_pole_sph_trans
!
!     Written by H. Matsui on July, 2007
!
!      subroutine resize_work_pole_sph_trans
!
!      subroutine allocate_work_pole_sph_trans
!      subroutine deallocate_work_pole_sph_trans
!      subroutine check_vr_pole(my_rank, nb)
!
!   compressed array
!   input /outpt arrays
!
!      radial component:      vr_rtp(3*i_rtp-2)
!      elevetional component: vr_rtp(3*i_rtp-1)
!      azimuthal component:   vr_rtp(2*i_rtp  )
!
!      Poloidal component:          sp_rj(3*i_rj-2)
!      diff. of Poloidal component: sp_rj(3*i_rj-1)
!      Toroidal component:          sp_rj(3*i_rj  )
!
!  transform for scalar
!   input /outpt arrays
!
!      field: vr_rtp(i_rtp)
!      spectr: sp_rj(i_rj)
!
      module m_work_pole_sph_trans
!
      use m_precision
!
      implicit none
!
!
      real(kind = kreal), allocatable :: v_np_local(:)
      real(kind = kreal), allocatable :: v_sp_local(:)
      real(kind = kreal), allocatable :: v_ct_local(:)
!
      real(kind = kreal), allocatable :: v_n_pole(:)
      real(kind = kreal), allocatable :: v_s_pole(:)
      real(kind = kreal), allocatable :: v_center(:)
!
      integer(kind = kint), private :: iflag_pole_trans = -1
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine resize_work_pole_sph_trans
!
      use m_work_4_sph_trans
!
      if (ncomp_sph_trans .gt. iflag_pole_trans) then
        call deallocate_work_pole_sph_trans
        call allocate_work_pole_sph_trans
        return
      end if
!
      if (iflag_pole_trans .le. 0)  call allocate_work_pole_sph_trans
!
      end subroutine resize_work_pole_sph_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine allocate_work_pole_sph_trans
!
      use m_phys_constants
      use m_spheric_parameter
      use m_work_4_sph_trans
!
      integer(kind = kint) :: num1
!
      num1 = ncomp_sph_trans*nidx_global_rtp(1)
      allocate(v_np_local(n_vector*num1) )
      allocate(v_sp_local(n_vector*num1) )
      allocate(v_ct_local(n_vector*ncomp_sph_trans) )
!
      allocate(v_n_pole(n_vector*num1) )
      allocate(v_s_pole(n_vector*num1) )
      allocate(v_center(n_vector*ncomp_sph_trans) )
!
      iflag_pole_trans = ncomp_sph_trans
!
      end subroutine allocate_work_pole_sph_trans
!
! ----------------------------------------------------------------------
!
      subroutine deallocate_work_pole_sph_trans
!
!
      deallocate(v_np_local, v_sp_local, v_ct_local)
      deallocate(v_n_pole, v_s_pole, v_center)
      iflag_pole_trans = 0
!
      end subroutine deallocate_work_pole_sph_trans
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine check_vr_pole(my_rank, nb)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank, nb
      integer(kind = kint) :: inod, ist, ied
!
      write(50+my_rank,*) 'v_n_pole', nb
      do inod = 1, nidx_global_rtp(1)
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(i10,1p200e20.12)') inod, v_n_pole(ist:ied)
      end do
!
      write(50+my_rank,*) 'v_s_pole', nb
      do inod = 1, nidx_global_rtp(1)
        ist = (inod-1) * nb + 1
        ied = (inod-1) * nb + nb
        write(50+my_rank,'(i10,1p200e20.12)') inod, v_s_pole(ist:ied)
      end do
!
      write(50+my_rank,*) 'v_center', nb
      ist = nb + 1
      ied = (inod-1) * nb + nb
      write(50+my_rank,'(1p200e20.12)') v_center(1:nb)
!
      end subroutine check_vr_pole
!
! ----------------------------------------------------------------------
!
      end module m_work_pole_sph_trans
