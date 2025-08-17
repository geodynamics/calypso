!>@file   t_pick_copy_monitor_data.f90
!!        program t_pick_copy_monitor_data
!!
!! @author H. Matsui
!! @date   Programmed in  Nov., 2007
!!
!!
!> @brief Append mean square data
!!
!!@verbatim
!!      subroutine open_bwd_serch_to_append(file_name, id_file,         &
!!     &          istep_start, start_time, nline_snap)
!!        character(len=kchara), intent(in) :: file_name
!!        integer(kind = kint), intent(in) :: id_file
!!        integer(kind = kint), intent(in) :: nline_snap
!!
!!        integer(kind = kint), intent(inout) :: istep_start
!!        real(kind = kreal) , intent(inout):: start_time
!!      subroutine copy_sph_monitor_to_end(FPz_f, id_stream, flag_gzip, &
!!     &                                   id_write, nline_snap, zbuf)
!!        character, pointer, intent(in) :: FPz_f
!!        integer(kind = kint), intent(in) :: id_stream, id_write
!!        logical, intent(in) :: flag_gzip
!!        integer(kind = kint), intent(in) :: nline_snap
!!        type(buffer_4_gzip), intent(inout) :: zbuf
!!      subroutine pick_copy_monitor_data                               &
!!     &         (comp_tbl, ntot_comp_in, ntot_comp_out,                &
!!     &          n_mode, spectr_in, spectr_out)
!!        type(monitor_field_pickup_table), intent(in) :: comp_tbl
!!        integer(kind = kint), intent(in) :: n_mode
!!        integer(kind = kint), intent(in) :: ntot_comp_in, ntot_comp_out
!!        real(kind = kreal), intent(in)                                &
!!       &                    :: spectr_in(ntot_comp_in,n_mode)
!!        real(kind = kreal), intent(inout)                             &
!!       &                   :: spectr_out(ntot_comp_out,n_mode)
!!
!!      subroutine init_pick_copy_sph_pwr_list                          &
!!     &        (ntot_sph_spec_in, ntot_sph_spec_out,                   &
!!     &         spectr_name_in, spectr_name_out, comp_tbl)
!!      subroutine dealloc_monitor_fld_pickup_tbl(comp_tbl)
!!        integer(kind = kint), intent(in) :: ntot_sph_spec_in
!!        integer(kind = kint), intent(in) :: ntot_sph_spec_out
!!        character(len = kchara), intent(in)                           &
!!     &                        :: spectr_name_in(ntot_sph_spec_in)
!!        character(len = kchara), intent(in)                           &
!!     &                        :: spectr_name_out(ntot_sph_spec_out)
!!        type(monitor_field_pickup_table), intent(inout) :: comp_tbl
!!@endverbatim
      module t_pick_copy_monitor_data
!
      use m_precision
      use m_constants
      use m_machine_parameter
      use t_read_sph_spectra
!
      implicit none
!
      type monitor_field_pickup_table
        logical :: fast_flag
        integer(kind = kint), allocatable :: icomp_in_read(:)
      end type monitor_field_pickup_table
!
      private :: alloc_monitor_fld_pickup_tbl
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine open_bwd_serch_to_append(file_name, id_file,           &
     &          istep_start, start_time, nline_snap)
!
      use count_monitor_time_series
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), intent(in) :: id_file
      integer(kind = kint), intent(in) :: nline_snap
!
      integer(kind = kint), intent(inout) :: istep_start
      real(kind = kreal) , intent(inout):: start_time
!
      integer(kind = kint) :: i_step, istep_end, ic, ierr
      real(kind = kreal) :: time, end_time
!
      integer(kind = kint) :: icou
!
!
        open(id_file, file=file_name, position='append')
        backspace(id_file)
        call read_ascii_sph_spectr_time(id_file, ione,                  &
     &                                  istep_end, end_time, ierr)
        if(ierr .gt. 0) then
          write(*,*) 'There is no data.'
          return
        end if
!
        write(*,*) 'end step and time for target file',                 &
     &          istep_end, end_time
        write(*,*) 'start step and time for append file',               &
     &          istep_start, start_time
!
        icou = 0
        if(istep_start .le. istep_end) then
          do
            backspace(id_file)
            call read_ascii_sph_spectr_time(id_file, ione,              &
     &                                      i_step, time, ierr)
            write(*,'(78a1,a5,i12,a4,1pe16.8e3,a29,i12)',advance="NO")  &
     &          (char(8),ic=1,78), 'step ', i_step,                     &
     &          ' at ', time, ' is read. Backeward count is  ', icou
            if(i_step .lt. istep_start) exit
            if(ierr .gt. 0) exit
            do ic = 1, nline_snap
              backspace(id_file)
            end do
            icou = icou + 1
          end do
        end if
        write(*,*)
!
      end subroutine  open_bwd_serch_to_append
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine copy_sph_monitor_to_end(FPz_f, id_stream, flag_gzip,   &
     &                                   id_write, nline_snap, zbuf)
!
      use gz_spl_sph_spectr_data_IO
!
      character, pointer, intent(in) :: FPz_f
      integer(kind = kint), intent(in) :: id_stream, id_write
      logical, intent(in) :: flag_gzip
      integer(kind = kint), intent(in) :: nline_snap
!
      type(buffer_4_gzip), intent(inout) :: zbuf
!
      integer(kind = kint) :: j, icou, ic, ierr
!
!
      icou = 0
      do
        icou = icou + 1
        do j = 1, nline_snap
          call copy_sph_monitor_line_to_text(FPz_f, id_stream,          &
     &        id_write, flag_gzip, zbuf, ierr)
          if(ierr .gt. 0) go to 99
        end do
!
        write(*,'(33a1,i12,a21)',advance="NO")                          &
     &      (char(8),ic=1,33), icou, '-th step is appended.'
      end do
!
  99  continue
      write(*,*)
!
      end subroutine copy_sph_monitor_to_end
!
! -----------------------------------------------------------------------
!
      subroutine pick_copy_monitor_data                                 &
     &         (comp_tbl, ntot_comp_in, ntot_comp_out,                  &
     &          n_mode, spectr_in, spectr_out)
!
      type(monitor_field_pickup_table), intent(in) :: comp_tbl
      integer(kind = kint), intent(in) :: n_mode
      integer(kind = kint), intent(in) :: ntot_comp_in, ntot_comp_out
      real(kind = kreal), intent(in) :: spectr_in(ntot_comp_in,n_mode)
      real(kind = kreal), intent(inout)                                 &
     &                   :: spectr_out(ntot_comp_out,n_mode)
!
      integer(kind = kint) :: nd, md, i
!
!
      do nd = 1, ntot_comp_out
        if(comp_tbl%icomp_in_read(nd) .eq. 0) cycle
        md = comp_tbl%icomp_in_read(nd)
!$omp parallel do
        do i = 1, n_mode
          spectr_out(nd,i) = spectr_in(md,i)
        end do
!$omp end parallel do
      end do
!
      end subroutine pick_copy_monitor_data
!
! -----------------------------------------------------------------------
! -----------------------------------------------------------------------
!
      subroutine init_pick_copy_sph_pwr_list                            &
     &        (ntot_sph_spec_in, ntot_sph_spec_out,                     &
     &         spectr_name_in, spectr_name_out, comp_tbl)
!
      integer(kind = kint), intent(in) :: ntot_sph_spec_in
      integer(kind = kint), intent(in) :: ntot_sph_spec_out
      character(len = kchara), intent(in)                               &
     &                        :: spectr_name_in(ntot_sph_spec_in)
      character(len = kchara), intent(in)                               &
     &                        :: spectr_name_out(ntot_sph_spec_out)
!
      type(monitor_field_pickup_table), intent(inout) :: comp_tbl
!
      integer(kind = kint) :: nd, md
!
!
      comp_tbl%fast_flag = .TRUE.
      if(ntot_sph_spec_in .ne. ntot_sph_spec_out) then
        comp_tbl%fast_flag = .FALSE.
      end if
!
      if(comp_tbl%fast_flag) then
        do nd = 1, ntot_sph_spec_out
          if(spectr_name_in(nd) .ne. spectr_name_out(nd)) then
            write(*,*) nd, '-th components in files does not match',    &
     &            trim(spectr_name_in(nd)), '    ',                     &
     &            trim(spectr_name_out(nd))
            comp_tbl%fast_flag = .FALSE.
            exit
          end if
        end do
      end if
      if(comp_tbl%fast_flag) return
!
      call alloc_monitor_fld_pickup_tbl(ntot_sph_spec_out, comp_tbl)
      do nd = 1, ntot_sph_spec_out
        do md = 1, ntot_sph_spec_in
          if(spectr_name_in(md) .eq. spectr_name_out(nd)) then
            comp_tbl%icomp_in_read(nd) = md
            exit
          end if
        end do
      end do
!        write(*,*) 'icomp_in_read', comp_tbl%icomp_in_read
!
      end subroutine init_pick_copy_sph_pwr_list
!
! -----------------------------------------------------------------------
!
      subroutine dealloc_monitor_fld_pickup_tbl(comp_tbl)
!
      type(monitor_field_pickup_table), intent(inout) :: comp_tbl
!
      if(allocated(comp_tbl%icomp_in_read) .eqv. .FALSE.) return
      deallocate(comp_tbl%icomp_in_read)
!
      end subroutine dealloc_monitor_fld_pickup_tbl
!
! -----------------------------------------------------------------------
!
      subroutine alloc_monitor_fld_pickup_tbl(n_comp, comp_tbl)
!
      integer(kind = kint), intent(in) :: n_comp
      type(monitor_field_pickup_table), intent(inout) :: comp_tbl
!
!
      allocate(comp_tbl%icomp_in_read(n_comp))
!
!$omp parallel workshare
      comp_tbl%icomp_in_read(1:n_comp) = 0
!$omp end parallel workshare
!
      end subroutine alloc_monitor_fld_pickup_tbl
!
! -----------------------------------------------------------------------
!
      end module t_pick_copy_monitor_data
