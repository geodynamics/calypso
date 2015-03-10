!
!      module m_rms_4_sph_spectr
!
!     Written by H. Matsui on Feb., 2008
!
!!      subroutine allocate_rms_name_sph_spec
!!      subroutine allocate_rms_4_sph_spectr(my_rank)
!!      subroutine deallocate_rms_4_sph_spectr(my_rank)
!!
!!      subroutine write_sph_vol_pwr(istep, time)
!!      subroutine write_sph_all_layer_pwr(istep, time)
!!      subroutine write_sph_selected_layer_pwr(istep, time,            &
!!     &          num_pick_layer, id_pick_layer)
!!      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label)
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
      module m_rms_4_sph_spectr
!
      use m_precision
!
      implicit none
!
!
      integer(kind = kint), parameter :: id_file_rms_l =    31
      integer(kind = kint), parameter :: id_file_rms_m =    32
      integer(kind = kint), parameter :: id_file_rms_lm =   33
      integer(kind = kint), parameter :: id_file_rms =      34
!
      integer (kind=kint) :: num_rms_rj
      integer (kind=kint) :: ntot_rms_rj
      integer (kind=kint), allocatable :: ifield_rms_rj(:)
      integer (kind=kint), allocatable :: num_rms_comp_rj(:)
      integer (kind=kint), allocatable :: istack_rms_comp_rj(:)
      character (len=kchara), allocatable :: rms_name_rj(:)
!
      integer (kind=kint) :: nri_rms
      real(kind = kreal), allocatable :: rms_sph_l(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_m(:,:,:)
      real(kind = kreal), allocatable :: rms_sph_lm(:,:,:)
      real(kind = kreal), allocatable :: rms_sph(:,:)
!
      real(kind = kreal), allocatable :: rms_sph_vol_l(:,:)
      real(kind = kreal), allocatable :: rms_sph_vol_m(:,:)
      real(kind = kreal), allocatable :: rms_sph_vol_lm(:,:)
      real(kind = kreal), allocatable :: rms_sph_vol(:)
!
!    output flag
!
      integer(kind = kint) :: iflag_layer_rms_spec =  0
      integer(kind = kint) :: iflag_volume_rms_spec = 0
      integer(kind = kint) :: iflag_volume_ave_sph =  0
!
!      data file name
!
      character(len = kchara) :: fhead_ave_vol =    'sph_ave_volume'
      character(len = kchara) :: fhead_rms_vol =    'sph_pwr_volume'
      character(len = kchara) :: fhead_rms_layer =  'sph_pwr_layer'
!
      private :: write_sph_mean_sq_header, set_sph_rms_labels
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_name_sph_spec
!
!
      allocate(ifield_rms_rj(num_rms_rj))
      allocate(num_rms_comp_rj(num_rms_rj))
      allocate(istack_rms_comp_rj(0:num_rms_rj))
      allocate(rms_name_rj(num_rms_rj))
!
      if (num_rms_rj .gt. 0) then
        num_rms_comp_rj = 0
        ifield_rms_rj =   0
      end if
      istack_rms_comp_rj = 0
!
      end subroutine allocate_rms_name_sph_spec
!
! -----------------------------------------------------------------------
!
      subroutine allocate_rms_4_sph_spectr(my_rank)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      nri_rms =  nidx_rj(1) 
!
      if(my_rank .gt. 0) return
!
      allocate( rms_sph_l(0:nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_m(0:nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_lm(0:nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph(0:nri_rms,ntot_rms_rj) )
      rms_sph = 0.0d0
      rms_sph_l =  0.0d0
      rms_sph_m =  0.0d0
      rms_sph_lm = 0.0d0
!
      allocate( rms_sph_vol_l(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vol_m(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vol_lm(0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_vol(ntot_rms_rj) )
      rms_sph_vol_l = 0.0d0
      rms_sph_vol_m = 0.0d0
      rms_sph_vol_lm = 0.0d0
      rms_sph_vol =    0.0d0
!
      end subroutine allocate_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rms_4_sph_spectr(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      if(my_rank .gt. 0) return
      deallocate( rms_sph_l, rms_sph_m, rms_sph_lm)
      deallocate( rms_sph )
!
      deallocate(rms_sph_vol_l, rms_sph_vol_m, rms_sph_vol_lm)
      deallocate(rms_sph_vol)
!
      deallocate(num_rms_comp_rj, istack_rms_comp_rj)
      deallocate(rms_name_rj, ifield_rms_rj)
!
      end subroutine deallocate_rms_4_sph_spectr
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine write_sph_vol_pwr(istep, time)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: lm
!
!
      do lm = 0, l_truncation
        write(id_file_rms_l,'(i16,1pe23.14e3,i16,1p200e23.14e3)')       &
     &            istep, time, lm, rms_sph_vol_l(lm,1:ntot_rms_rj)
        write(id_file_rms_m,'(i16,1pe23.14e3,i16,1p200e23.14e3)')       &
     &            istep, time, lm, rms_sph_vol_m(lm,1:ntot_rms_rj)
        write(id_file_rms_lm,'(i16,1pe23.14e3,i16,1p200e23.14e3)')      &
     &            istep, time, lm, rms_sph_vol_lm(lm,1:ntot_rms_rj)
      end do
!
      end subroutine write_sph_vol_pwr
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_all_layer_pwr(istep, time)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: kst, kg, lm
!
      kst = 1
      if(iflag_rj_center .gt. 0) kst = 0
!
      do kg = kst, nidx_rj(1)
        write(id_file_rms,'(i16,1pe23.14e3,i16,1p200e23.14e3)')         &
     &                   istep, time, kg, rms_sph(kg,1:ntot_rms_rj)
      end do
!
      do kg = kst, nidx_rj(1)
        do lm = 0, l_truncation
          write(id_file_rms_l,'(i16,1pe23.14e3,2i16,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_l(kg,lm,1:ntot_rms_rj)
          write(id_file_rms_m,'(i16,1pe23.14e3,2i16,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_m(kg,lm,1:ntot_rms_rj)
          write(id_file_rms_lm,'(i16,1pe23.14e3,2i16,1p200e23.14e3)')   &
     &           istep, time, kg, lm, rms_sph_lm(kg,lm,1:ntot_rms_rj)
        end do
      end do
!
      end subroutine write_sph_all_layer_pwr
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_selected_layer_pwr(istep, time,              &
     &          num_pick_layer, id_pick_layer)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: num_pick_layer
      integer(kind = kint), intent(in) :: id_pick_layer(num_pick_layer)
      integer(kind = kint), intent(in) :: istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: k, kg, lm
!
!
      do k = 1, num_pick_layer
        kg = id_pick_layer(k)
        write(id_file_rms,'(i16,1pe23.14e3,i16,1p200e23.14e3)')         &
     &                   istep, time, kg, rms_sph(kg,1:ntot_rms_rj)
      end do
!
      do k = 1, num_pick_layer
        kg = id_pick_layer(k)
        do lm = 0, l_truncation
          write(id_file_rms_l,'(i16,1pe23.14e3,2i16,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_l(kg,lm,1:ntot_rms_rj)
          write(id_file_rms_m,'(i16,1pe23.14e3,2i16,1p200e23.14e3)')    &
     &           istep, time, kg, lm, rms_sph_m(kg,lm,1:ntot_rms_rj)
          write(id_file_rms_lm,'(i16,1pe23.14e3,2i16,1p200e23.14e3)')   &
     &           istep, time, kg, lm, rms_sph_lm(kg,lm,1:ntot_rms_rj)
        end do
      end do
!
      end subroutine write_sph_selected_layer_pwr
!
! -----------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label)
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: fname_rms, mode_label
!
!
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='old', position='append', err = 99)
      return
!
   99 continue
      open(id_file, file=fname_rms, form='formatted',                   &
     &    status='replace')
      call write_sph_mean_sq_header(id_file, mode_label)
!
      end subroutine open_sph_mean_sq_file
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_mean_sq_header(id_file, mode_label)
!
      use m_spheric_parameter
      use m_phys_labels
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint) :: i, nri
!
      character(len=kchara) :: labels(6)
!
!
      nri = nidx_rj(1)
      if(iflag_rj_center .gt. 0) nri = nri + 1
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') nri, l_truncation
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(3i16)') nlayer_ICB, nlayer_CMB
!
      write(id_file,'(a)')    'number of components'
      write(id_file,'(5i16)')   num_rms_rj, ntot_rms_rj
      write(id_file,'(16i5)')   num_rms_comp_rj(1:num_rms_rj)
!
!
      write(id_file,'(a)',advance='no')    't_step    time    '
      if(mode_label .ne. 'EMPTY') then
        write(id_file,'(a,a4)',advance='no') trim(mode_label), '    '
      end if
!
      do i = 1, num_rms_rj
        call set_sph_rms_labels(num_rms_comp_rj(i), rms_name_rj(i),     &
     &      labels(1))
        call write_multi_labels(id_file, num_rms_comp_rj(i), labels(1))
      end do
      write(id_file,*)
!
      end subroutine write_sph_mean_sq_header
!
!  --------------------------------------------------------------------
!
      subroutine set_sph_rms_labels(num_rms_comp, rms_name, labels)
!
      use m_phys_labels
      use add_direction_labels
!
      integer(kind = kint), intent(in) :: num_rms_comp
      character(len = kchara), intent(in) :: rms_name
!
      character(len = kchara), intent(inout) :: labels(num_rms_comp)
!
!
      if ( rms_name .eq. fhd_velo) then
        write(labels(1),'(a)')   'K_ene_pol'
        write(labels(2),'(a)')   'K_ene_tor'
        write(labels(3),'(a)')   'K_ene'
!
      else if (rms_name .eq. fhd_magne) then
        write(labels(1),'(a)')   'M_ene_pol'
        write(labels(2),'(a)')   'M_ene_tor'
        write(labels(3),'(a)')   'M_ene'
!
      else if (rms_name .eq. fhd_filter_v) then
        write(labels(1),'(a)')   'filter_KE_pol'
        write(labels(2),'(a)')   'filter_KE_tor'
        write(labels(3),'(a)')   'filter_KE'
!
      else if (rms_name .eq. fhd_filter_b) then
        write(labels(1),'(a)')   'filter_ME_pol'
        write(labels(2),'(a)')   'filter_ME_tor'
        write(labels(3),'(a)')   'filter_ME'
!
      else if (num_rms_comp .eq. 1) then
        write(labels(1),'(a)')   trim(rms_name)
      else if (num_rms_comp .eq. 3) then
        call add_vector_power_sph_label(rms_name,                       &
     &          labels(1), labels(2), labels(3))
      else if (num_rms_comp .eq. 6) then
        call add_tensor_direction_label_rtp(rms_name,                   &
     &          labels(1), labels(2), labels(3), labels(4), labels(5),  &
     &          labels(6))
      end if
!
      end subroutine set_sph_rms_labels
!
!  --------------------------------------------------------------------
!
      end module m_rms_4_sph_spectr
