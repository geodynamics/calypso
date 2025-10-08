!>@file   compare_sph_mean_square.f90
!!        program compare_sph_mean_square
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!
!>@brief Compare volume mean square data from reference
!!@n
!!@n      Reference data: reference/sph_pwr_volumne.dat
!!@n      Compared data:          ./sph_pwr_volumne.dat
!!
!
      program compare_sph_mean_square
!
      use m_precision
      use skip_comment_f
      use t_buffer_4_gzip
      use t_read_sph_spectra
      use t_sph_spectr_head_labels
!
      use compare_sph_monitor_header
      use sel_gz_input_sph_mtr_head
      use select_gz_stream_file_IO
      use gz_spl_sph_spectr_data_IO
      use set_parallel_file_name
      use skip_comment_f
!
      implicit none
!
      character(len = kchara) :: fhead_rms_vol, fname_rms_vol
      character(len = kchara) :: fhead_rms_ref, fname_rms_ref
!
      character(len = kchara), parameter :: exclude = 'pressure'
!
      real(kind = kreal) :: error_limit = 1.0d-10
!
      integer(kind = kint), parameter :: id_file1 = 34, id_file2 = 36
      character, pointer :: FPz_f1, FPz_f2
      type(buffer_4_gzip) :: zbuf1, zbuf2
      type(read_sph_spectr_data) :: sph_IN1, sph_IN2
      type(sph_spectr_head_labels) :: sph_lbl_IN1, sph_lbl_IN2
      logical :: flag_gzip1, flag_gzip2
      logical :: flag_miss1, flag_miss2
      character(len = kchara) :: file_name
!
      logical :: error
      integer(kind = kint) :: ierr1, ierr2
      real(kind = kreal), allocatable :: spectr_IN1(:)
      real(kind = kreal), allocatable :: spectr_IN2(:)
      real(kind = kreal) :: diff, max_diff
      real(kind = kreal) :: d1_maxdiff, d2_maxdiff
      integer(kind = kint) :: icomp_maxdiff, istep_maxdiff
!
      integer(kind = kint) :: icou
      integer(kind = kint) :: icomp1, icomp2
!
      integer(kind = kint) :: iflag_gl = 0
      character(len = kchara) :: charaint
!
      integer(kind = kint) :: num_compare
      integer(kind = kint), allocatable :: id_comp(:,:)
      character(len = kchara), allocatable :: cmp_name(:)
!
!
      if(command_argument_count() .le. 1) then
        write(*,*) 'sph_ene_check ',                                    &
     &             'REFERENCE_FILE_PREFIX COMPARED_FILE_PREFIX'
        stop
      end if
      call get_command_argument(1, fhead_rms_ref)
      call get_command_argument(2, fhead_rms_vol)
      fname_rms_ref = add_dat_extension(fhead_rms_ref)
      fname_rms_vol = add_dat_extension(fhead_rms_vol)
!
!
      call sel_open_check_gz_stream_file(FPz_f1, id_file1,              &
     &   fname_rms_vol, flag_gzip1, flag_miss1, file_name, zbuf1)
      if(flag_miss1) then
        write(*,*) 'Data file ', trim(fname_rms_vol), ' is missing.'
        error = .TRUE.
        go to 99
      end if
      call read_sph_volume_mean_head(FPz_f1, id_file1, flag_gzip1,      &
     &                               sph_lbl_IN1, sph_IN1, zbuf1)
!
      call sel_open_check_gz_stream_file(FPz_f2, id_file2,              &
     &   fname_rms_ref, flag_gzip2, flag_miss2, file_name, zbuf2)
      if(flag_miss2) then
        write(*,*) 'Data file ', trim(fname_rms_ref), ' is missing.'
        error = .TRUE.
        go to 99
      end if
      call read_sph_volume_mean_head(FPz_f2, id_file2, flag_gzip2,      &
     &                               sph_lbl_IN2, sph_IN2, zbuf2)
!
      error = .not. cmp_sph_volume_monitor_heads                        &
     &            (sph_lbl_IN1, sph_IN1, sph_lbl_IN2, sph_IN2)
      if(error) then
        write(*,*) 'Time sequence data header does not match.'
        write(*,*) 'Check failed'
        go to 99
      end if
!
!      write(*,*) 'sph_IN1%ene_sph_spec_name', sph_IN1%num_labels,       &
!     &          sph_IN1%ntot_sph_spec
!      do icomp1 = 1, sph_IN1%num_labels
!        write(*,*) icomp1, trim(sph_IN1%ene_sph_spec_name(icomp1))
!      end do
!      write(*,*) 'sph_IN2%ene_sph_spec_name', sph_IN2%num_labels        &
!     &          sph_IN2%ntot_sph_spec
!      do icomp1 = 1, sph_IN2%num_labels
!        write(*,*) icomp1, trim(sph_IN2%ene_sph_spec_name(icomp1))
!      end do
!
      num_compare = count_compare_table(sph_IN1, sph_IN2, exclude)
      allocate(cmp_name(num_compare))
      allocate(id_comp(num_compare,2))
      call set_compare_table(sph_IN1, sph_IN2, exclude,                 &
     &                       num_compare, id_comp, cmp_name, error)
      if(error) then
        write(*,*) 'There is missing field.'
        write(*,*) 'Check failed'
        go to 99
      end if
!
      allocate(spectr_IN1(sph_IN1%ntot_sph_spec))
      allocate(spectr_IN2(sph_IN2%ntot_sph_spec))
      do
        call gz_read_volume_pwr_sph(FPz_f1, id_file1, flag_gzip1,       &
     &      sph_IN1%ntot_sph_spec, sph_IN1%i_step, sph_IN1%time,        &
     &      spectr_IN1(1), zbuf1, ierr1)
        call gz_read_volume_pwr_sph(FPz_f2, id_file2, flag_gzip2,       &
     &      sph_IN2%ntot_sph_spec, sph_IN2%i_step, sph_IN2%time,        &
     &      spectr_IN2(1), zbuf2, ierr2)
        if(ierr1*ierr2 .gt. 0) exit
!
        if(ierr1+ierr1 .gt. 0 .and. ierr1*ierr2 .eq. 0) then
          write(*,*) 'Read fails in either file'
          error = .TRUE.
          go to 99
        end if
!
        error = .FALSE.
        do icou = 1, num_compare
          icomp1 = id_comp(icou,1)
          icomp2 = id_comp(icou,2)
          diff = compare_data(spectr_IN1(icomp1), spectr_IN2(icomp2))
!
          if(diff .eq. zero) cycle
!
          if(diff .gt. max_diff) then
            max_diff = diff
            icomp_maxdiff = icou
            istep_maxdiff = sph_IN1%i_step
            d1_maxdiff = spectr_IN1(icomp1)
            d2_maxdiff = spectr_IN2(icomp2)
          end if
!
          if(abs(diff) .gt. error_limit) then
            write(*,*) 'Large error in ', trim(cmp_name(icou)),         &
     &           ' at step ', sph_IN1%i_step,                           &
     &           ': ', spectr_IN1(icomp1), spectr_IN2(icomp2), diff
            error = .TRUE.
            go to 99
          end if
        end do
      end do
!
  99  continue
      write(*,*) 'Maxmum difference at step ', istep_maxdiff
      write(*,*) 'Maxmum difference in ', trim(cmp_name(icomp_maxdiff))
      write(*,*) 'Relative difference: ', max_diff
      write(*,*) 'data: ', d1_maxdiff, d2_maxdiff
!
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f1, id_file1, flag_gzip1, zbuf1)
      call sel_close_read_gz_stream_file                                &
     &   (FPz_f2, id_file2, flag_gzip2, zbuf2)
!
      if(error) then
        write(*,*) 'Time sequence data file ', trim(fname_rms_ref),     &
     &            ' and ', trim(fhead_rms_vol), ' does not match.'
        iflag_gl = 1
      else
        iflag_gl = 0
      end if
!
      open(999,file='flag.txt')
      write(charaint,*) iflag_gl
      write(999,'(a)') trim(ADJUSTL(charaint))
      close(999)
!
      stop
!
!   --------------------------------------------------------------------
!
      contains
!
!   --------------------------------------------------------------------
!   --------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &            count_compare_table(sph_IN, sph_OUT, exclude)
!
      type(read_sph_spectr_data) :: sph_IN, sph_OUT
      character(len = kchara), intent(in) :: exclude
!
      integer(kind = kint) :: ist, icou
      integer(kind = kint) :: icomp1, icomp2
!
      icou = 0
      ist = sph_IN%num_time_labels
      do icomp1 = 1, sph_IN%ntot_sph_spec
        if(trim(sph_IN%ene_sph_spec_name(icomp1+ist))                   &
     &                                  .eq. trim(exclude)) cycle
        do icomp2 = 1, sph_OUT%ntot_sph_spec
          if(sph_IN%ene_sph_spec_name(icomp1+ist)                       &
     &       .eq. sph_OUT%ene_sph_spec_name(icomp2+ist)) then
            icou = icou + 1
            exit
          end if
        end do
      end do
      count_compare_table = icou
!
      end function count_compare_table
!
!   --------------------------------------------------------------------
!
      subroutine set_compare_table(sph_IN, sph_OUT, exclude,            &
     &          num_compare, id_comp, cmp_name, error)
!
      type(read_sph_spectr_data) :: sph_IN, sph_OUT
      character(len = kchara), intent(in) :: exclude
      integer(kind = kint), intent(in) :: num_compare
!
      integer(kind = kint), intent(inout) :: id_comp(num_compare,2)
      character(len = kchara), intent(inout) :: cmp_name(num_compare)
      logical, intent(inout) :: error
!
      integer(kind = kint) :: ist, icou
      integer(kind = kint) :: icomp1, icomp2
!
      error = .FALSE.
      icou = 0
      ist = sph_IN%num_time_labels
      do icomp1 = 1, sph_IN%ntot_sph_spec
        if(trim(sph_IN%ene_sph_spec_name(icomp1+ist))                   &
     &                                  .eq. trim(exclude)) cycle
        do icomp2 = 1, sph_OUT%ntot_sph_spec
          if(sph_IN%ene_sph_spec_name(icomp1+ist)                       &
     &       .eq. sph_OUT%ene_sph_spec_name(icomp2+ist)) then
            icou = icou + 1
            cmp_name(icou) = sph_IN%ene_sph_spec_name(icomp1+ist)
            id_comp(icou,1) = icomp1
            id_comp(icou,2) = icomp2
            go to 10
          end if
        end do
        error = .TRUE.
        write(*,*) trim(sph_IN%ene_sph_spec_name(icomp1+ist)),          &
     &            ' is missing.'
  10    continue
      end do
!
      end subroutine set_compare_table
!
!   --------------------------------------------------------------------
!
      end program compare_sph_mean_square
