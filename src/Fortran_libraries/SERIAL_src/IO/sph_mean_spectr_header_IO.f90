!>@file   sph_mean_spectr_header_IO.f90
!!@brief  module sph_mean_spectr_header_IO
!!
!!@author H. Matsui
!!@date Programmed in Feb., 2008
!
!>@brief Mean sqare data
!!
!!@verbatim
!!      subroutine write_sph_vol_mean_sq_header                         &
!!     &         (id_file, mode_label, ltr,                             &
!!     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,&
!!     &          nri, nlayer_ICB, nlayer_CMB, kr_inner, kr_outer,      &
!!     &          r_inner,  r_outer)
!!      subroutine write_sph_mean_sq_header(id_file, mode_label, ltr,   &
!!     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,&
!!     &          nri_rms, nlayer_ICB, nlayer_CMB)
!!
!!      integer(kind = kint)  function check_sph_vol_mean_sq_header     &
!!     &         (id_file, mode_label, ltr,                             &
!!     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,&
!!     &          nri, nlayer_ICB, nlayer_CMB, kr_inner, kr_outer)
!!
!!      subroutine set_sph_rms_labels(num_rms_comp, rms_name, labels)
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
      module sph_mean_spectr_header_IO
!
      use m_precision
      use m_phys_labels
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine write_sph_vol_mean_sq_header                           &
     &         (id_file, mode_label, ltr,                               &
     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,  &
     &          nri, nlayer_ICB, nlayer_CMB, kr_inner, kr_outer,        &
     &          r_inner,  r_outer)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: nri, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_inner, kr_outer
      real(kind = kreal), intent(in) ::   r_inner,  r_outer
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
      integer(kind = kint) :: i
      character(len=kchara) :: labels(6)
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') nri, ltr
      write(id_file,'(a)')    'ICB_id, CMB_id'
      write(id_file,'(2i16)') nlayer_ICB, nlayer_CMB
      write(id_file,'(a)')    'Lower boudary'
      write(id_file,'(i16,1pe23.14e3)') kr_inner, r_inner
      write(id_file,'(a)')    'Upper boundary'
      write(id_file,'(i16,1pe23.14e3)') kr_outer, r_outer
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
      end subroutine write_sph_vol_mean_sq_header
!
!  --------------------------------------------------------------------
!
      subroutine write_sph_mean_sq_header(id_file, mode_label, ltr,     &
     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,  &
     &          nri_rms, nlayer_ICB, nlayer_CMB)
!
      use write_field_labels
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: nri_rms, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
      integer(kind = kint) :: i
      character(len=kchara) :: labels(6)
!
!
      write(id_file,'(a)')    'radial_layers, truncation'
      write(id_file,'(3i16)') nri_rms, ltr
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
      integer(kind = kint)  function check_sph_vol_mean_sq_header       &
     &         (id_file, mode_label, ltr,                               &
     &          num_rms_rj, ntot_rms_rj, num_rms_comp_rj, rms_name_rj,  &
     &          nri, nlayer_ICB, nlayer_CMB, kr_inner, kr_outer)
!
      use write_field_labels
      use skip_comment_f
!
      integer(kind = kint), intent(in) :: id_file
      character(len = kchara), intent(in) :: mode_label
      integer(kind = kint), intent(in) :: nri, ltr
      integer(kind = kint), intent(in) :: num_rms_rj, ntot_rms_rj
      integer(kind = kint), intent(in) :: nlayer_ICB, nlayer_CMB
      integer(kind = kint), intent(in) :: kr_inner, kr_outer
      integer(kind = kint), intent(in) :: num_rms_comp_rj(num_rms_rj)
      character (len=kchara), intent(in) :: rms_name_rj(num_rms_rj)
!
      integer(kind = kint) :: nri_read, ltr_read
      integer(kind = kint) :: nfld_read, ntot_read
      integer(kind = kint) :: nlayer_ICB_read, nlayer_CMB_read
      integer(kind = kint) :: kr_inner_read, kr_outer_read
      integer(kind = kint), allocatable :: num_read_comp(:)
      character (len=kchara), allocatable :: read_name(:)
!
      character(len=255) :: character_4_read
      character(len=kchara) :: labels(6), tmpc1, tmpc2, tmpc3
      integer(kind = kint) :: i, nd, icou
      real(kind = kreal) :: rtmp
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nri_read, ltr_read
      if(nri_read .ne. nri) then
        write(*,*) 'Truncation does not match ',                        &
     &             'with the data in the file'
        check_sph_vol_mean_sq_header = 1
        return
      end if
!
      if(ltr_read .ne. ltr) then
        write(*,*) 'Inner boundary address does not match ',            &
     &             'with the data in the file'
        check_sph_vol_mean_sq_header = 1
        return
      end if
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nlayer_ICB_read, nlayer_CMB_read
      if(nlayer_ICB_read .ne. nlayer_ICB) then
        write(*,*) 'Inner boundary address does not match ',            &
     &             'with the data in the file'
        check_sph_vol_mean_sq_header = 1
        return
      end if
!
      if(nlayer_CMB_read .ne. nlayer_CMB) then
        write(*,*) 'Outer boundary address does not match ',            &
     &             'with the data in the file'
        check_sph_vol_mean_sq_header = 1
        return
      end if
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) kr_inner_read, rtmp
      if(kr_inner_read .ne. kr_inner) then
        write(*,*) 'Inner area address does not match ',                &
     &             'with the data in the file'
        check_sph_vol_mean_sq_header = 1
        return
      end if
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) kr_outer_read, rtmp
      if(kr_outer_read .ne. kr_outer) then
        write(*,*) 'Outer area address does not match ',                &
     &             'with the data in the file'
        check_sph_vol_mean_sq_header = 1
        return
      end if
!
!
      call skip_comment(character_4_read, id_file)
      read(id_file,*) nfld_read, ntot_read
      if(nfld_read .ne. num_rms_rj) then
        write(*,*) 'Number of fields does not match ',                  &
     &             'with the data in the file'
        check_sph_vol_mean_sq_header = 1
        return
      end if
!
      if(ntot_read .ne. ntot_rms_rj) then
        write(*,*) 'total Number of components does not match ',        &
     &             'with the data in the file'
        check_sph_vol_mean_sq_header = 1
        return
      end if
!
!
      allocate(num_read_comp(nfld_read))
      allocate(read_name(ntot_read))
!
      read(id_file,*) num_read_comp(1:nfld_read)
!
      if(mode_label .ne. 'EMPTY') then
        read(id_file,*) tmpc1, tmpc2, tmpc3, read_name(1:ntot_read)
      else
        read(id_file,*) tmpc1, tmpc2, read_name(1:ntot_read)
      end if
!
      icou = 0
      do i = 1, num_rms_rj
        call set_sph_rms_labels(num_rms_comp_rj(i), rms_name_rj(i),     &
     &      labels(1))
!
        if(num_read_comp(i) .ne. num_rms_comp_rj(i)) then
          write(*,*) 'Number of component for ', trim(rms_name_rj(i)),  &
     &               'does not match with the data file'
          check_sph_vol_mean_sq_header = 1
          deallocate(read_name, num_read_comp)
          return
        end if
!
        do nd = 1, num_rms_comp_rj(i)
          icou = icou + 1
          if(read_name(icou) .ne. labels(nd)) then
            write(*,*) 'field ', trim(labels(nd)),                      &
     &                 ' does not match with the data file',            &
     &                 read_name(icou), labels(nd)
            check_sph_vol_mean_sq_header = 1
            deallocate(read_name, num_read_comp)
            return
          end if
        end do
      end do
!
      deallocate(read_name, num_read_comp)
      check_sph_vol_mean_sq_header = 0
!
      end function check_sph_vol_mean_sq_header
!
!  --------------------------------------------------------------------
!  --------------------------------------------------------------------
!
      subroutine set_sph_rms_labels(num_rms_comp, rms_name, labels)
!
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
      else if (rms_name .eq. fhd_filter_velo) then
        write(labels(1),'(a)')   'filter_KE_pol'
        write(labels(2),'(a)')   'filter_KE_tor'
        write(labels(3),'(a)')   'filter_KE'
!
      else if (rms_name .eq. fhd_filter_magne) then
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
      end module sph_mean_spectr_header_IO
      