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
!!      subroutine alloc_rms_4_sph_spectr(id_rank, ltr, pwr)
!!      subroutine alloc_ave_4_sph_spectr                               &
!!     &         (idx_rj_degree_zero, nri_rj, pwr)
!!
!!      subroutine dealloc_rms_4_sph_spectr(pwr)
!!      subroutine dealloc_ave_4_sph_spectr(idx_rj_degree_zero, pwr)
!!@endverbatim
!!
!!@n @param id_rank       Process ID
!
      module t_rms_4_sph_spectr
!
      use m_precision
!
      use t_sph_volume_mean_square
!
      implicit none
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
        integer (kind=kint), pointer :: num_comp_sq(:)
!>        End ID of each field for mean square
        integer (kind=kint), allocatable :: istack_comp_sq(:)
!>        Field name for mean square
        character (len=kchara), pointer :: pwr_name(:)
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
!>        MPI rank for l-spectr data output
        integer :: irank_l
!>        MPI rank for m-spectr data output
        integer :: irank_m
!>        MPI rank for l-m -spectr data output
        integer :: irank_lm
!
!>        Number of radial points for mean square
        integer(kind=kint) :: nri_rms
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
      integer(kind = kint) :: i
!
!
      pwr%num_vol_spectr = n_vpower
      allocate(pwr%v_spectr(n_vpower))
!
      do i = 1, pwr%num_vol_spectr
        write(pwr%v_spectr(i)%fhead_rms_v,'(a)') 'sph_pwr_volume'
        write(pwr%v_spectr(i)%fhead_rms_v,'(a)') 'sph_ave_volume'
!
        pwr%v_spectr(i)%irank_l =  -1
        pwr%v_spectr(i)%irank_m =  -1
        pwr%v_spectr(i)%irank_lm = -1
!
        pwr%v_spectr(i)%iflag_volume_rms_spec = 0
        pwr%v_spectr(i)%iflag_volume_ave_sph = 0
!
        pwr%v_spectr(i)%ltr = 0
        pwr%v_spectr(i)%ntot_comp_sq = 0
!
        pwr%v_spectr(i)%r_inside =  0
        pwr%v_spectr(i)%r_outside = 0
        pwr%v_spectr(i)%kr_inside =  0.0d0
        pwr%v_spectr(i)%kr_outside = 0.0d0
      end do
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
      subroutine alloc_rms_4_sph_spectr(id_rank, ltr, pwr)
!
      integer, intent(in) :: id_rank
      integer(kind = kint), intent(in) :: ltr
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i
!
      pwr%ntot_comp_sq = pwr%istack_comp_sq(pwr%num_fld_sq)
!
      do i = 1, pwr%num_vol_spectr
        call alloc_sph_vol_mean_square(id_rank, ltr,                    &
     &      pwr%num_fld_sq, pwr%ntot_comp_sq, pwr%v_spectr(i))
!
        pwr%v_spectr(i)%num_comp_sq =>    pwr%num_comp_sq
        pwr%v_spectr(i)%pwr_name =>       pwr%pwr_name
      end do
!
      if(id_rank .eq. pwr%irank_l) then
        allocate(pwr%shl_l(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
        if(pwr%nri_rms .gt. 0) pwr%shl_l =  0.0d0
      else
        allocate(pwr%shl_l(0,0,0))
      end if
!
      if(id_rank .eq. pwr%irank_lm) then
        allocate(pwr%shl_lm(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
        if(pwr%nri_rms .gt. 0) pwr%shl_lm = 0.0d0
      else
        allocate(pwr%shl_lm(0,0,0))
      end if
!
      if(id_rank .eq. pwr%irank_m) then
        allocate(pwr%shl_m(pwr%nri_rms,0:ltr,pwr%ntot_comp_sq))
        if(pwr%nri_rms .gt. 0) pwr%shl_m =        0.0d0
!
        allocate( pwr%shl_m0(pwr%nri_rms,pwr%ntot_comp_sq) )
        if(pwr%nri_rms .gt. 0) pwr%shl_m0 =       0.0d0
!
        allocate( pwr%ratio_shl_m0(pwr%nri_rms,pwr%ntot_comp_sq) )
        if(pwr%nri_rms .gt. 0) pwr%ratio_shl_m0 = 0.0d0
      else
        allocate(pwr%shl_m(0,0,0))
        allocate(pwr%shl_m0(0,0))
        allocate(pwr%ratio_shl_m0(0,0))
      end if
!
      if(     id_rank.eq.pwr%irank_l .or. id_rank.eq.pwr%irank_m        &
     &   .or. id_rank.eq.pwr%irank_lm) then
        allocate( pwr%shl_sq(pwr%nri_rms,pwr%ntot_comp_sq) )
        if(pwr%nri_rms .gt. 0) pwr%shl_sq =       0.0d0
      else
        allocate(pwr%shl_sq(0,0))
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
! -----------------------------------------------------------------------
!
      subroutine dealloc_rms_4_sph_spectr(pwr)
!
      type(sph_mean_squares), intent(inout) :: pwr
!
      integer(kind = kint) :: i
!
      do i = 1, pwr%num_vol_spectr
        nullify(pwr%v_spectr(i)%num_comp_sq)
        nullify(pwr%v_spectr(i)%pwr_name)
!
        call dealloc_sph_vol_mean_square(pwr%v_spectr(i))
      end do
!
      deallocate(pwr%r_4_rms, pwr%kr_4_rms)
!
!
      deallocate(pwr%shl_l)
      deallocate(pwr%shl_lm)
      deallocate(pwr%shl_m, pwr%shl_m0, pwr%ratio_shl_m0)
      deallocate(pwr%shl_sq)
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
!
      end module t_rms_4_sph_spectr
