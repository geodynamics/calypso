!>@file   t_rms_4_sph_spectr.f90
!!@brief  module t_rms_4_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine alloc_volume_spectr_data(n_vpower, pwr)
!!      subroutine dealloc_volume_spectr_data(pwr)
!!
!!      subroutine alloc_num_spec_layer(nri_in, pwr)
!!      subroutine alloc_rms_name_sph_spec(nfld_in, pwr)
!!      subroutine alloc_rms_4_sph_spectr(my_rank, ltr, pwr)
!!      subroutine alloc_ave_4_sph_spectr                               &
!!     &         (idx_rj_degree_zero, nri_rj, pwr)
!!
!!      subroutine dealloc_rms_4_sph_spectr(my_rank, pwr)
!!      subroutine dealloc_ave_4_sph_spectr(idx_rj_degree_zero, pwr)
!!@endverbatim
!!
!!@n @param my_rank       Process ID
!!@n @param istep         time step number
!!@n @param time          time
!!
!!@n @param id_file       file ID for output
!!@n @param fname_rms     file name for output
!!@n @param mode_label    data label for degree or order of harmonics
!
      module t_rms_4_sph_spectr
!
      use m_precision
!
      implicit none
!
!
!>      Structure of mean square data on each surface
      type sphere_mean_squares
!>        Number of radial points for mean square
        integer(kind=kint) :: nri_rms = 0
!>        Number of radial points for mean square
        integer(kind=kint) :: ltr
!>        Number of component for mean square
        integer (kind=kint) :: ntot_comp_sq
!
!>        Radial ID from layered mean square
        integer(kind=kint), allocatable :: kr_rms(:)
!>        Radius from layered mean square
        real(kind = kreal), allocatable :: r_rms(:)
!
!>        Mean square spectrum for degree on spheres
        real(kind = kreal), allocatable :: s_l(:,:,:)
!>        Mean square spectrum for order on spheres
        real(kind = kreal), allocatable :: s_m(:,:,:)
!>        Mean square spectrum for l-m on spheres
        real(kind = kreal), allocatable :: s_lm(:,:,:)
!
!>         Mean square on spheres
        real(kind = kreal), allocatable :: s_sq(:,:)
!>         Mean square of axis-symmetric component on spheres
        real(kind = kreal), allocatable :: s_m0(:,:)
!>        Ratio of axis-symmetric componbent to total mean square
        real(kind = kreal), allocatable :: s_ratio_m0(:,:)
!
!>        Number of radial point for average
        integer(kind = kint) :: nri_ave
!>        Average over single sphere
        real(kind = kreal), allocatable :: s_ave(:,:)
      end type sphere_mean_squares
!
!
!>      Structure of mean square data over volume
      type sph_vol_mean_squares
!>        File prefix for volume mean square file
        character(len = kchara) :: fhead_rms_v = 'sph_pwr_volume'
!>        File prefix for volume average file
        character(len = kchara) :: fhead_ave = 'sph_ave_volume'
!
!>        Output flag for volume mean square data
        integer(kind = kint) :: iflag_volume_rms_spec = 0
!>        Output flag for volume average data
        integer(kind = kint) :: iflag_volume_ave_sph =  0
!
!>        Number of radial points for mean square
        integer(kind=kint) :: ltr
!>        Number of component for mean square
        integer (kind=kint) :: ntot_comp_sq
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
!
!>      Structure of mean square data
      type sph_mean_squares
!>        Number of field for mean square
        integer (kind=kint) :: num_fld_sq
!>        Number of component for mean square
        integer (kind=kint) :: ntot_comp_sq
!
!>        Output flag for layerd mean square data
        integer(kind = kint) :: iflag_layer_rms_spec =  0
!
!>        Field ID for mean square
        integer (kind=kint), allocatable :: id_field(:)
!>        Number of each component for mean square
        integer (kind=kint), allocatable :: num_comp_sq(:)
!>        End ID of each field for mean square
        integer (kind=kint), allocatable :: istack_comp_sq(:)
!>        Field name for mean square
        character (len=kchara), allocatable :: pwr_name(:)
!
!
!>        File prefix for layered mean square file
        character(len = kchara) :: fhead_rms_layer =  'sph_pwr_layer'
!
!>        Output flag for spectrum with respect to degree
        integer(kind = kint) :: iflag_spectr_l =  1
!>        Output flag for spectrum with respect to order
        integer(kind = kint) :: iflag_spectr_m =  1
!>        Output flag for spectrum with respect to l-m
        integer(kind = kint) :: iflag_spectr_lm = 1
!
!>        Output flag for spectrum for axis-symmetric component
        integer(kind = kint) :: iflag_spectr_m0 = 1
!
!>        Number of radial points for mean square
        integer(kind=kint) :: nri_rms = 0
!
!>        Radial ID from layered mean square
        integer(kind=kint), allocatable :: kr_4_rms(:)
!>        Radius from layered mean square
        real(kind = kreal), allocatable :: r_4_rms(:)
!
!>        Mean square spectrum for degree on spheres
        real(kind = kreal), allocatable :: shl_l(:,:,:)
!>      Mean square spectrum for order on spheres
        real(kind = kreal), allocatable :: shl_m(:,:,:)
!>        Mean square spectrum for l-m on spheres
        real(kind = kreal), allocatable :: shl_lm(:,:,:)
!
!>         Mean square on spheres
        real(kind = kreal), allocatable :: shl_sq(:,:)
!>         Mean square of axis-symmetric component on spheres
        real(kind = kreal), allocatable :: shl_m0(:,:)
!>        Ratio of axis-symmetric componbent to total mean square
        real(kind = kreal), allocatable :: ratio_shl_m0(:,:)
!
!
!>        Number of radial point for average
        integer(kind = kint) :: nri_ave
!>        Average over single sphere
        real(kind = kreal), allocatable :: shl_ave(:,:)
!>        Volume average
!        real(kind = kreal), allocatable :: vol_ave(:)
!
        integer(kind = kint) :: num_vol_spectr = 1
        type(sph_vol_mean_squares), allocatable :: v_spectr(:)
!v_spectr(1)%v_l
      end type sph_mean_squares
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine alloc_volume_spectr_data(n_vpower, pwr)
!
      integer(kind = kint), intent(in) ::n_vpower
      type(sph_mean_squares), intent(inout) :: pwr
!
      pwr%num_vol_spectr = n_vpower
      allocate(pwr%v_spectr(n_vpower))
!
      end subroutine alloc_volume_spectr_data
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_volume_spectr_data(pwr)
!
      type(sph_mean_squares), intent(inout) :: pwr
!
      deallocate(pwr%v_spectr)
!
      end subroutine dealloc_volume_spectr_data
!
! -----------------------------------------------------------------------
!
      subroutine alloc_num_spec_layer(nri_in, pwr)
!
      integer(kind = kint), intent(in) :: nri_in
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      pwr%nri_rms = nri_in
!
      allocate( pwr%kr_4_rms(pwr%nri_rms) )
      allocate( pwr%r_4_rms(pwr%nri_rms) )
      if(pwr%nri_rms .gt. 0) then
        pwr%kr_4_rms = 0
        pwr%r_4_rms =  0.0d0
      end if
!
      end subroutine alloc_num_spec_layer
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rms_name_sph_spec(nfld_in, pwr)
!
      integer(kind = kint), intent(in) :: nfld_in
      type(sph_mean_squares), intent(inout) :: pwr
!
!
      pwr%num_fld_sq = nfld_in
      allocate(pwr%id_field(pwr%num_fld_sq))
      allocate(pwr%num_comp_sq(pwr%num_fld_sq))
      allocate(pwr%istack_comp_sq(0:pwr%num_fld_sq))
      allocate(pwr%pwr_name(pwr%num_fld_sq))
!
      if (pwr%num_fld_sq .gt. 0) then
        pwr%num_comp_sq = 0
        pwr%id_field =   0
      end if
      pwr%istack_comp_sq = 0
!
      end subroutine alloc_rms_name_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine alloc_rms_4_sph_spectr(my_rank, ltr, pwr)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: ltr
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i
!
      pwr%ntot_comp_sq = pwr%istack_comp_sq(pwr%num_fld_sq)
      do i = 1, pwr%num_vol_spectr
        call alloc_sph_vol_mean_square                                  &
     &     (my_rank, ltr, pwr%ntot_comp_sq, pwr%v_spectr(i))
      end do
!
      if(my_rank .gt. 0) return
!
      allocate(pwr%shl_l(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
      allocate(pwr%shl_m(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
      allocate(pwr%shl_lm(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
!
      allocate( pwr%shl_sq(pwr%nri_rms,pwr%ntot_comp_sq) )
      allocate( pwr%shl_m0(pwr%nri_rms,pwr%ntot_comp_sq) )
      allocate( pwr%ratio_shl_m0(pwr%nri_rms,pwr%ntot_comp_sq) )
      if(pwr%nri_rms .gt. 0) then
        pwr%shl_sq =       0.0d0
        pwr%shl_m0 =       0.0d0
        pwr%ratio_shl_m0 = 0.0d0
!
        pwr%shl_l =  0.0d0
        pwr%shl_m =  0.0d0
        pwr%shl_lm = 0.0d0
      end if
!
      end subroutine alloc_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine alloc_ave_4_sph_spectr                                 &
     &         (idx_rj_degree_zero, nri_rj, pwr)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nri_rj
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i
!
      do i = 1, pwr%num_vol_spectr
        call alloc_sph_vol_ave(idx_rj_degree_zero, pwr%v_spectr(i))
      end do
!
      if(idx_rj_degree_zero .eq. 0) return
!
      pwr%nri_ave = nri_rj
      allocate(pwr%shl_ave(0:pwr%nri_ave,pwr%ntot_comp_sq))
      if(pwr%nri_ave*pwr%ntot_comp_sq .gt. 0)  pwr%shl_ave = 0.0d0
!
      end subroutine alloc_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_rms_4_sph_spectr(my_rank, pwr)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i
!
      do i = 1, pwr%num_vol_spectr
        call dealloc_sph_vol_mean_square(my_rank, pwr%v_spectr(i))
      end do
!
      deallocate(pwr%r_4_rms, pwr%kr_4_rms)
!
      if(my_rank .gt. 0) return
      deallocate(pwr%shl_l, pwr%shl_m, pwr%shl_lm)
      deallocate(pwr%shl_sq, pwr%shl_m0, pwr%ratio_shl_m0)
!
      deallocate(pwr%num_comp_sq, pwr%istack_comp_sq)
      deallocate(pwr%pwr_name, pwr%id_field)
!
      end subroutine dealloc_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_ave_4_sph_spectr(idx_rj_degree_zero, pwr)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i
!
      do i = 1, pwr%num_vol_spectr
        call dealloc_sph_vol_ave(idx_rj_degree_zero, pwr%v_spectr(i))
      end do
!
      if(idx_rj_degree_zero .eq. 0) return
      deallocate(pwr%shl_ave)
!
      end subroutine dealloc_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_layer_mean_square                            &
     &         (my_rank, ltr, ntot_comp_sq, s_rms)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: ltr, ntot_comp_sq
      type(sphere_mean_squares), intent(inout) :: s_rms
!
!
      s_rms%ltr = ltr
      s_rms%ntot_comp_sq = ntot_comp_sq
      if(my_rank .gt. 0) return
!
      allocate(s_rms%s_l(s_rms%nri_rms,0:s_rms%ltr,ntot_comp_sq))
      allocate(s_rms%s_m(s_rms%nri_rms,0:s_rms%ltr,ntot_comp_sq))
      allocate(s_rms%s_lm(s_rms%nri_rms,0:s_rms%ltr,ntot_comp_sq))
!
      allocate(s_rms%s_sq(s_rms%nri_rms,ntot_comp_sq))
      allocate(s_rms%s_m0(s_rms%nri_rms,ntot_comp_sq))
      allocate(s_rms%s_ratio_m0(s_rms%nri_rms,ntot_comp_sq))
      if(s_rms%nri_rms .gt. 0) then
        s_rms%s_sq =       0.0d0
        s_rms%s_m0 =       0.0d0
        s_rms%s_ratio_m0 = 0.0d0
!
        s_rms%s_l =  0.0d0
        s_rms%s_m =  0.0d0
        s_rms%s_lm = 0.0d0
      end if
!
      end subroutine alloc_sph_layer_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_vol_mean_square                              &
     &         (my_rank, ltr, ntot_comp_sq, v_pwr)
!
      integer(kind = kint), intent(in) :: my_rank
      integer(kind = kint), intent(in) :: ltr, ntot_comp_sq
      type(sph_vol_mean_squares), intent(inout) :: v_pwr
!
!
      v_pwr%ltr = ltr
      v_pwr%ntot_comp_sq = ntot_comp_sq
      if(my_rank .gt. 0) return
!
      allocate( v_pwr%v_l(0:v_pwr%ltr,v_pwr%ntot_comp_sq) )
      allocate( v_pwr%v_m(0:v_pwr%ltr,v_pwr%ntot_comp_sq) )
      allocate( v_pwr%v_lm(0:v_pwr%ltr,v_pwr%ntot_comp_sq) )
!
      allocate( v_pwr%v_sq(v_pwr%ntot_comp_sq) )
      allocate( v_pwr%v_m0(v_pwr%ntot_comp_sq) )
      allocate( v_pwr%v_ratio_m0(v_pwr%ntot_comp_sq) )
!
      v_pwr%v_l = 0.0d0
      v_pwr%v_m =  0.0d0
      v_pwr%v_lm = 0.0d0
!
      v_pwr%v_sq =       0.0d0
      v_pwr%v_m0 =       0.0d0
      v_pwr%v_ratio_m0 = 0.0d0
!
      end subroutine alloc_sph_vol_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine alloc_sph_layer_ave                                    &
     &         (idx_rj_degree_zero, nri_rj, s_rms)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      integer(kind = kint), intent(in) :: nri_rj
      type(sphere_mean_squares), intent(inout) :: s_rms
!
!
      if(idx_rj_degree_zero .eq. 0) return
!
      s_rms%nri_ave = nri_rj
      allocate(s_rms%s_ave(0:s_rms%nri_ave,s_rms%ntot_comp_sq))
      if(s_rms%nri_ave*s_rms%ntot_comp_sq .gt. 0)  s_rms%s_ave = 0.0d0
!
      end subroutine alloc_sph_layer_ave
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
      subroutine dealloc_num_spec_layer(s_rms)
!
      type(sphere_mean_squares), intent(inout) :: s_rms
!
!
      deallocate(s_rms%kr_rms, s_rms%r_rms)
!
      end subroutine dealloc_num_spec_layer
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_layer_mean_square(my_rank, s_rms)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sphere_mean_squares), intent(inout) :: s_rms
!
!
      if(my_rank .gt. 0) return
!
      deallocate(s_rms%s_l, s_rms%s_m, s_rms%s_lm)
      deallocate(s_rms%s_sq, s_rms%s_m0, s_rms%s_ratio_m0)
!
      end subroutine dealloc_sph_layer_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_vol_mean_square(my_rank, v_pwr)
!
      integer(kind = kint), intent(in) :: my_rank
      type(sph_vol_mean_squares), intent(inout) :: v_pwr
!
!
      if(my_rank .gt. 0) return
!
      deallocate(v_pwr%v_l, v_pwr%v_m, v_pwr%v_lm)
      deallocate(v_pwr%v_sq, v_pwr%v_m0, v_pwr%v_ratio_m0)
!
      end subroutine dealloc_sph_vol_mean_square
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_sph_layer_ave(idx_rj_degree_zero, s_rms)
!
      integer(kind = kint), intent(in) :: idx_rj_degree_zero
      type(sphere_mean_squares), intent(inout) :: s_rms
!
!
      if(idx_rj_degree_zero .gt. 0)  deallocate(s_rms%s_ave)
!
      end subroutine dealloc_sph_layer_ave
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
!
      end module t_rms_4_sph_spectr
