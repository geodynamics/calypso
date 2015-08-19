!>@file   m_rms_4_sph_spectr.f90
!!@brief  module m_rms_4_sph_spectr
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine allocate_num_spec_layer
!!      subroutine allocate_rms_name_sph_spec
!!      subroutine allocate_rms_4_sph_spectr(my_rank)
!!      subroutine allocate_ave_4_sph_spectr(nri_ave)
!!      subroutine deallocate_rms_4_sph_spectr(my_rank)
!!      subroutine deallocate_ave_4_sph_spectr
!!
!!      subroutine open_sph_mean_sq_file(id_file, fname_rms, mode_label)
!!      subroutine write_sph_volume_data(id_file, istep, time, rms_sph_x)
!!      subroutine write_sph_layerd_power(id_file, istep, time)
!!      subroutine write_sph_layer_data(id_file, istep, time, rms_sph_x)
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
!>      Number of field for mean square
      integer (kind=kint) :: num_rms_rj
!
!>      Number of component for mean square
      integer (kind=kint) :: ntot_rms_rj
!
!>      Field ID for mean square
      integer (kind=kint), allocatable :: ifield_rms_rj(:)
!
!>      Number of each component for mean square
      integer (kind=kint), allocatable :: num_rms_comp_rj(:)
!
!>      End ID of each field for mean square
      integer (kind=kint), allocatable :: istack_rms_comp_rj(:)
!
!>      Field name for mean square
      character (len=kchara), allocatable :: rms_name_rj(:)
!
!
!>      Number of radial points for mean square
      integer(kind=kint) :: nri_rms = 0
!
!>      Radial ID from layered mean square
      integer(kind=kint), allocatable :: kr_for_rms(:)
!
!>      Radius from layered mean square
      real(kind = kreal), allocatable :: r_for_rms(:)
!
!>      Mean square spectrum for degree on spheres
      real(kind = kreal), allocatable :: rms_sph_l(:,:,:)
!
!>      Mean square spectrum for order on spheres
      real(kind = kreal), allocatable :: rms_sph_m(:,:,:)
!
!>      Mean square spectrum for l-m on spheres
      real(kind = kreal), allocatable :: rms_sph_lm(:,:,:)
!
!>       Mean square on spheres
      real(kind = kreal), allocatable :: rms_sph(:,:)
!
!
!>      Volume mean square spectrum for degree
      real(kind = kreal), allocatable :: rms_sph_vol_l(:,:)
!
!>      Volume mean square spectrum for order
      real(kind = kreal), allocatable :: rms_sph_vol_m(:,:)
!
!>      Volume mean square spectrum for l-m
      real(kind = kreal), allocatable :: rms_sph_vol_lm(:,:)
!
!>      Volume mean square
      real(kind = kreal), allocatable :: rms_sph_vol(:)
!
!
!>      Number of radial point for average
      integer(kind = kint) :: nri_ave
!
!>      Average over single sphere
      real(kind = kreal), allocatable :: ave_sph(:,:)
!
!>      Volume average
      real(kind = kreal), allocatable :: ave_sph_vol(:)
!
      private :: write_sph_mean_sq_header, set_sph_rms_labels
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine allocate_num_spec_layer
!
!
      allocate( kr_for_rms(nri_rms) )
      allocate( r_for_rms(nri_rms) )
      if(nri_rms .gt. 0) then
        kr_for_rms = 0
        r_for_rms =  0.0d0
      end if
!
      end subroutine allocate_num_spec_layer
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
      if(my_rank .gt. 0) return
!
      allocate( rms_sph_l(nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_m(nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph_lm(nri_rms,0:l_truncation,ntot_rms_rj) )
      allocate( rms_sph(nri_rms,ntot_rms_rj) )
      if(nri_rms .gt. 0) then
        rms_sph = 0.0d0
        rms_sph_l =  0.0d0
        rms_sph_m =  0.0d0
        rms_sph_lm = 0.0d0
      end if
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
      subroutine allocate_ave_4_sph_spectr
!
      use m_spheric_parameter
!
      if(idx_rj_degree_zero .eq. 0) return
!
!
      nri_ave = nidx_rj(1)
      allocate(ave_sph_vol(ntot_rms_rj))
      allocate(ave_sph(0:nri_ave,ntot_rms_rj))
!
      if(nri_ave*ntot_rms_rj .gt. 0) then
        ave_sph=     0.0d0
        ave_sph_vol = 0.0d0
      end if
!
      end subroutine allocate_ave_4_sph_spectr
!
! -----------------------------------------------------------------------
!
      subroutine deallocate_rms_4_sph_spectr(my_rank)
!
      integer(kind = kint), intent(in) :: my_rank
!
!
      deallocate(r_for_rms, kr_for_rms)
!
      if(my_rank .gt. 0) return
      deallocate(rms_sph_l, rms_sph_m, rms_sph_lm, rms_sph)
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
!
      subroutine deallocate_ave_4_sph_spectr
!
      use m_spheric_parameter
!
      if(idx_rj_degree_zero .eq. 0) return
      deallocate(ave_sph, ave_sph_vol)
!
      end subroutine deallocate_ave_4_sph_spectr
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
      integer(kind = kint) :: i
!
      character(len=kchara) :: labels(6)
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') nri_rms, l_truncation
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
!  --------------------------------------------------------------------
!
      subroutine write_sph_volume_data(id_file, istep, time, rms_sph_x)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(0:l_truncation, ntot_rms_rj)
!
      integer(kind = kint) :: lm
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,', ntot_rms_rj, '(1pe23.14e3),a1)'
!
      do lm = 0, l_truncation
        write(id_file,fmt_txt) istep, time, lm,                         &
     &                         rms_sph_x(lm,1:ntot_rms_rj)
      end do
!
      end subroutine write_sph_volume_data
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layerd_power(id_file, istep, time)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
!
      integer(kind = kint) :: k, kg
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a20,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,i16,', ntot_rms_rj, '(1pe23.14e3),a1)'
      do k = 1, nri_rms
        kg = kr_for_rms(k)
        write(id_file,fmt_txt) istep, time, kg,                         &
     &                         rms_sph(k,1:ntot_rms_rj)
      end do
!
      end subroutine write_sph_layerd_power
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_layer_data(id_file, istep, time, rms_sph_x)
!
      use m_spheric_parameter
!
      integer(kind = kint), intent(in) :: id_file, istep
      real(kind = kreal), intent(in) :: time
      real(kind = kreal), intent(in)                                    &
     &      :: rms_sph_x(nri_rms,0:l_truncation, ntot_rms_rj)
!
      integer(kind = kint) :: k, kg, lm
      character(len=kchara) :: fmt_txt
!
!
      write(fmt_txt,'(a21,i3,a16)')                                     &
     &     '(i16,1pe23.14e3,2i16,', ntot_rms_rj, '(1pe23.14e3),a1)'
!
      do k = 1, nri_rms
        kg = kr_for_rms(k)
        do lm = 0, l_truncation
          write(id_file,fmt_txt) istep, time, kg, lm,                   &
     &                           rms_sph_x(k,lm,1:ntot_rms_rj)
        end do
      end do
!
      end subroutine write_sph_layer_data
!
! -----------------------------------------------------------------------
!
      end module m_rms_4_sph_spectr
