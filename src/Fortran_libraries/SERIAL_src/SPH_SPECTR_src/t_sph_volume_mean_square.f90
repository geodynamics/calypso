!>@file   t_sph_volume_mean_square.f90
!!@brief  module t_sph_volume_mean_square
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine alloc_sph_vol_mean_square                            &
!!     &         (id_rank, ltr, num_fld_sq, ntot_comp_sq, v_pwr)
!!      subroutine alloc_sph_vol_ave(idx_rj_degree_zero, v_pwr)
!!        type(sph_vol_mean_squares), intent(inout) :: v_pwr
!!
!!      subroutine dealloc_sph_vol_mean_square(v_pwr)
!!      subroutine dealloc_sph_vol_ave(idx_rj_degree_zero, v_pwr)
!!        type(sph_vol_mean_squares), intent(inout) :: v_pwr
!!@endverbatim
!!
!!@n @param id_rank       Process ID
!
      module t_sph_volume_mean_square
!
      use m_precision
!
      implicit none
!
!
!>      Structure of mean square data over volume
      type sph_vol_mean_squares
!>        File prefix for volume mean square file
        character(len = kchara) :: fhead_rms_v
!>        File prefix for volume average file
        character(len = kchara) :: fhead_ave
!
!>        MPI rank for l-spectr data output
        integer :: irank_l
!>        MPI rank for m-spectr data output
        integer :: irank_m
!>        MPI rank for l-m -spectr data output
        integer :: irank_lm
!
!>        Output flag for volume mean square data
        integer(kind = kint) :: iflag_volume_rms_spec
!>        Output flag for volume average data
        integer(kind = kint) :: iflag_volume_ave_sph
!
!>        Number of radial points for mean square
        integer(kind=kint) :: ltr
!
!>        Number of field for mean square
        integer (kind=kint) :: num_fld_sq
!>        Number of component for mean square
        integer (kind=kint) :: ntot_comp_sq
!>        Number of each component for mean square
        integer (kind=kint), pointer :: num_comp_sq(:)
!>        Field name for mean square
        character (len=kchara), pointer :: pwr_name(:)
!
!>        Radius for inner boundary
        real(kind=kreal) :: r_inside
!>        Radius for outer boundary
        real(kind=kreal) :: r_outside
!>        Radial address for inner boundary
        integer (kind=kint) :: kr_inside
!>        Radial address for outer boundary
        integer (kind=kint) :: kr_outside
!
!>        Volume mean square spectrum for degree
        real(kind = kreal), allocatable :: v_l(:,:)
!>        Volume mean square spectrum for order
        real(kind = kreal), allocatable :: v_m(:,:)
!>        Volume mean square spectrum for l-m
        real(kind = kreal), allocatable :: v_lm(:,:)
!
!>        Volume mean square
        real(kind = kreal), allocatable :: v_sq(:)
!>        Volume mean square of axis-symmetric component
        real(kind = kreal), allocatable :: v_m0(:)
!>        Ratio of axis-symmetric componbent to total mean square
        real(kind = kreal), allocatable :: v_ratio_m0(:)
!
!>        Volume average
        real(kind = kreal), allocatable :: v_ave(:)
!>        1 / volume
        real(kind = kreal) :: avol
      end type sph_vol_mean_squares
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_vol_mean_square                              &
     &         (id_rank, ltr, num_fld_sq, ntot_comp_sq, v_pwr)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ltr, num_fld_sq, ntot_comp_sq
      type(sph_vol_mean_squares), intent(inout) :: v_pwr
!
!
      v_pwr%ltr = ltr
      v_pwr%num_fld_sq = num_fld_sq
      v_pwr%ntot_comp_sq = ntot_comp_sq
!
      if(id_rank .eq. v_pwr%irank_l) then
        allocate( v_pwr%v_l(0:v_pwr%ltr,v_pwr%ntot_comp_sq) )
        v_pwr%v_l = 0.0d0
      else
        allocate( v_pwr%v_l(0,0) )
      end if
!
      if(id_rank .eq. v_pwr%irank_lm) then
        allocate( v_pwr%v_lm(0:v_pwr%ltr,v_pwr%ntot_comp_sq) )
        v_pwr%v_lm = 0.0d0
      else
        allocate( v_pwr%v_lm(0,0) )
      end if
!
      if(id_rank .eq. v_pwr%irank_m) then
        allocate( v_pwr%v_m(0:v_pwr%ltr,v_pwr%ntot_comp_sq) )
        v_pwr%v_m =  0.0d0
!
        allocate( v_pwr%v_m0(v_pwr%ntot_comp_sq) )
        v_pwr%v_m0 = 0.0d0
!
        allocate( v_pwr%v_ratio_m0(v_pwr%ntot_comp_sq) )
        v_pwr%v_ratio_m0 = 0.0d0
      else
        allocate( v_pwr%v_m(0,0) )
        allocate( v_pwr%v_m0(0) )
        allocate( v_pwr%v_ratio_m0(0) )
      end if
!
      if(     id_rank.eq.v_pwr%irank_l .or. id_rank.eq.v_pwr%irank_m    &
     &   .or. id_rank.eq.v_pwr%irank_lm) then
        allocate( v_pwr%v_sq(v_pwr%ntot_comp_sq) )
        v_pwr%v_sq =       0.0d0
      else
        allocate( v_pwr%v_sq(0) )
      end if
!
      end subroutine alloc_sph_vol_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_vol_ave(idx_rj_degree_zero, v_pwr)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      type(sph_vol_mean_squares), intent(inout) :: v_pwr
!
!
      if(idx_rj_degree_zero .eq. 0) return
!
      allocate(v_pwr%v_ave(v_pwr%ntot_comp_sq))
!
      if(v_pwr%ntot_comp_sq .gt. 0) v_pwr%v_ave = 0.0d0
!
      end subroutine alloc_sph_vol_ave
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_vol_mean_square(v_pwr)
!
      type(sph_vol_mean_squares), intent(inout) :: v_pwr
!
!
        deallocate(v_pwr%v_l)
        deallocate(v_pwr%v_lm)
        deallocate(v_pwr%v_m, v_pwr%v_m0, v_pwr%v_ratio_m0)
        deallocate(v_pwr%v_sq)
!
      end subroutine dealloc_sph_vol_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_vol_ave(idx_rj_degree_zero, v_pwr)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      type(sph_vol_mean_squares), intent(inout) :: v_pwr
!
!
      if(idx_rj_degree_zero .gt. 0)  deallocate(v_pwr%v_ave)
!
      end subroutine dealloc_sph_vol_ave
!
! -----------------------------------------------------------------------
!
      end module t_sph_volume_mean_square
