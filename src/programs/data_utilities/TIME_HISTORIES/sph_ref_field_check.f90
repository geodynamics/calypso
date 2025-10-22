!>@file   sph_ref_field_check.f90
!!@brief  program sph_ref_field_check
!!
!!@author H. Matsui
!!@date Programmed by H. Matsui in July 2014 
!
!>@brief  Main program to compare reference field output
      program sph_ref_field_check
!
      use m_precision
      use m_constants
!
      implicit none
!
      character(len = kchara) :: ref_name, file_name
!
!
      if(command_argument_count() .lt. 2) then
        write(*,*) 'Command [REFERENCE FILE NAME] [COMPARED FILE NAME]'
        stop
      end if
      call get_command_argument(1, ref_name)
      call get_command_argument(2, file_name)
!
      call analyze_compare_sph_reference(ref_name, file_name)
      stop '***** program finished *****'
!
! ----------------------------------------------------------------------
!
      contains
!
! ----------------------------------------------------------------------
!
      subroutine analyze_compare_sph_reference(file1_name, file2_name)
!
      use t_phys_data
      use t_time_data
      use t_field_data_IO
      use field_file_IO
      use copy_rj_phys_data_4_IO
      use append_phys_data
!
      implicit none
!
      character(len = kchara), intent(in) :: file1_name, file2_name
!
      type(phys_data) :: r_fld1, r_fld2
      type(time_data) :: t1_IO, t2_IO
      type(field_IO) :: fld1_IO, fld2_IO
      integer(kind = kint) :: iend
      integer(kind = kint) :: icount_error = 0
      real(kind = kreal) :: diff_max = 0.0d0
      character(len=kchara) :: charaint
!
!
      call read_and_alloc_step_field                                    &
     &   (file1_name, izero, t1_IO, fld1_IO, iend)
!
      call copy_rj_phys_name_from_IO(fld1_IO, r_fld1)
      call alloc_phys_data(fld1_IO%nnod_IO, r_fld1)
      call copy_rj_phys_data_from_IO(fld1_IO, r_fld1)
!
!
      call read_and_alloc_step_field                                    &
     &   (file2_name, izero, t2_IO, fld2_IO, iend)
      call copy_rj_phys_name_from_IO(fld2_IO, r_fld2)
      call alloc_phys_data(fld2_IO%nnod_IO, r_fld2)
      call copy_rj_phys_data_from_IO(fld2_IO, r_fld2)
!
      call compare_field_data(r_fld1, r_fld2, TINY,                     &
     &                        diff_max, icount_error)
!
!
      write(*,*) 'Maximum difference: ', diff_max
      if(icount_error.gt.0) then
        write(*,'(a)') 'Data do not have consistentency'
      else
        write(*,'(a)') 'Data have a consistecy.'
      end if
!
      open(999,file='flag.txt')
      write(charaint,*) icount_error
      write(999,'(a)') trim(ADJUSTL(charaint))
      close(999)
!
      end subroutine analyze_compare_sph_reference
!
! ----------------------------------------------------------------------
!
      end program sph_ref_field_check
