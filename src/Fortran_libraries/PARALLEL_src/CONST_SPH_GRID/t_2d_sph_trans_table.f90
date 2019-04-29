!>@file   t_2d_sph_trans_table.f90
!!@brief  module t_2d_sph_trans_table
!!
!!@author H. Matsui
!!@date Programmed in July, 2007
!
!>@brief  Indices of spherical transform for second harmonics
!!
!!@verbatim
!!      subroutine alloc_2d_sph_trans_table(sph_rtp, sph_rj, s2d_tbl)
!!      subroutine dealloc_2d_sph_trans_table(s2d_tbl)
!!        type(sph_rtp_grid), intent(in) :: sph_rtp
!!        type(sph_rj_grid), intent(in) :: sph_rj
!!        type(sph_trans_2d_table), intent(inout) :: s2d_tbl
!!
!!      subroutine check_2d_sph_indices                                 &
!!     &         (id_rank, nphi, ltr, jmax, s2d_tbl)
!!      subroutine check_2d_sph_trans_table                             &
!!     &         (id_rank, ntheta, nphi, s2d_tbl)
!!@endverbatim
!!
!!@param id_rank  Process ID
!!@param ntheta   Number of meridional grids
!!@param nphi     Number of zonal grids
!!@param ltr      Truncation degree
!!@param jmax     Number of spherical harmonics modes
!
      module t_2d_sph_trans_table
!
      use m_precision
!
      implicit none
!
!
      type sph_trans_2d_table
        integer(kind = kint), allocatable :: mdx_ispack(:)
!
        integer(kind = kint), allocatable :: jdx_fsph(:)
        integer(kind = kint), allocatable :: mspec_4_ispack(:)
        integer(kind = kint), allocatable :: mtbl_fft_2_lgd(:)
!
        integer(kind = kint), allocatable :: mdx_4_lgd(:)
        integer(kind = kint), allocatable :: jtbl_fsph(:,:)
!
        integer(kind = kint), allocatable :: jtbl_rj(:,:)
      end type sph_trans_2d_table
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_2d_sph_trans_table(sph_rtp, sph_rj, s2d_tbl)
!
      use t_spheric_parameter
!
      type(sph_rtp_grid), intent(in) :: sph_rtp
      type(sph_rj_grid), intent(in) :: sph_rj
      type(sph_trans_2d_table), intent(inout) :: s2d_tbl
!
      integer(kind = kint) :: ntheta, nphi, jmax
!
      ntheta = sph_rtp%nidx_global_rtp(2)
      nphi =   sph_rtp%nidx_global_rtp(3)
      jmax =   sph_rj%nidx_global_rj(2)
!
      allocate( s2d_tbl%jdx_fsph(-ntheta:ntheta) )
      allocate( s2d_tbl%mspec_4_ispack(-ntheta:ntheta) )
!
      allocate( s2d_tbl%mtbl_fft_2_lgd(0:nphi) )
      allocate( s2d_tbl%mdx_4_lgd(0:nphi) )
!
      allocate( s2d_tbl%mdx_ispack(nphi) )
!
      allocate( s2d_tbl%jtbl_rj(0:jmax,3)   )
      allocate( s2d_tbl%jtbl_fsph(0:jmax,3) )
!
      s2d_tbl%jdx_fsph = 0
      s2d_tbl%mspec_4_ispack = 0
      s2d_tbl%mtbl_fft_2_lgd = 0
      s2d_tbl%mdx_4_lgd = 0
      s2d_tbl%mdx_ispack = 0
!
      s2d_tbl%jtbl_rj =   0
      s2d_tbl%jtbl_fsph = 0
!
      end subroutine alloc_2d_sph_trans_table
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_2d_sph_trans_table(s2d_tbl)
!
      type(sph_trans_2d_table), intent(inout) :: s2d_tbl
!
!
      deallocate( s2d_tbl%jdx_fsph, s2d_tbl%mspec_4_ispack )
      deallocate( s2d_tbl%mtbl_fft_2_lgd, s2d_tbl%mdx_4_lgd )
!
      deallocate( s2d_tbl%mdx_ispack )
!
      deallocate( s2d_tbl%jtbl_rj, s2d_tbl%jtbl_fsph )
!
      end subroutine dealloc_2d_sph_trans_table
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine check_2d_sph_indices                                   &
     &         (id_rank, nphi, ltr, jmax, s2d_tbl)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: nphi, jmax, ltr
      type(sph_trans_2d_table), intent(in) :: s2d_tbl
!
      integer(kind = kint) :: m, j, l
!
!
      write(id_rank+50,*) 'zonal wave numbers after FFT'
      write(id_rank+50,*) 'm, mdx_ispack(m)'
      do m = 1, nphi
        write(id_rank+50,*) m, s2d_tbl%mdx_ispack(m)
      end do
!
      write(id_rank+50,*) 'zonal wave numbers before Legendre trans.'
      write(id_rank+50,*) 'm, s2d_tbl%mdx_4_lgd(m)'
      do m = 0, nphi
        write(id_rank+50,*) m, s2d_tbl%mdx_4_lgd(m)
      end do
!
      write(id_rank+50,*) 'spectr data after Legendre transform'
      write(id_rank+50,*) 'j, s2d_tbl%jtbl_fsph(j,1:3)'
      do j = 0, jmax
        write(id_rank+50,*) j, s2d_tbl%jtbl_fsph(j,1:3)
      end do
!
      write(id_rank+50,*) 'spectr data ordering for linear terms'
      write(id_rank+50,*) 'j, s2d_tbl%jtbl_fsph(j,1:3)'
      do j = 0, jmax
        write(id_rank+50,*) j, s2d_tbl%jtbl_rj(j,1:3)
      end do
!
      write(id_rank+50,*) 'spectr data for final distribution'
      write(id_rank+50,*) 'j, l, m'
      do l = 0, ltr
        do m = -l, l
          j = l*(l+1) + m
          write(id_rank+50,*) j, l, m
        end do
      end do
!
      end subroutine check_2d_sph_indices
!
! -----------------------------------------------------------------------
!
      subroutine check_2d_sph_trans_table                               &
     &         (id_rank, ntheta, nphi, s2d_tbl)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ntheta, nphi
      type(sph_trans_2d_table), intent(in) :: s2d_tbl
!
      integer(kind = kint) :: m
!
      write(id_rank+50,*) 'zonal wave number and tranfer table'
      write(id_rank+50,*) 'm, mspec_4_ispack(m), jdx_fsph(m)'
      do m = -ntheta, ntheta
        write(id_rank+50,*) m, s2d_tbl%mspec_4_ispack(m),               &
     &                         s2d_tbl%jdx_fsph(m)
      end do
!
      write(id_rank+50,*)                                               &
     &        'tranfer table from zonal spectr for Lag. trans.'
      write(id_rank+50,*) 'm0, mtbl_fft_2_lgd, mdx_4_lgd(m0)'
      do m = 0, nphi
        write(id_rank+50,*) m, s2d_tbl%mtbl_fft_2_lgd(m),               &
     &                         s2d_tbl%mdx_4_lgd(m)
      end do
!
      end subroutine check_2d_sph_trans_table
!
! -----------------------------------------------------------------------
!
      end module t_2d_sph_trans_table
