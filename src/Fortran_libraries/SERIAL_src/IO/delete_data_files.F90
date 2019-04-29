!>@file   delete_data_files.f90
!!@brief  module delete_data_files
!!
!!@author H. Matsui
!!@date Programmed in July, 2006
!!@n    modified in May, 2009
!
!>@brief Delete data files use close command
!!
!!@verbatim
!!      subroutine delete_file_if_exist(file_name)
!!      subroutine delete_file_by_f(file_name)
!!      subroutine delete_parallel_files(iflag_fmt, nprocs, file_head)
!!
!!      integer(kind = kint) function check_file_exist(file_name)
!!@endverbatim
!!
!!@n @param  file_name   file name
!!@n @param  file_head   file header to delete
!!@n @param  iflag_fmt   file format flag
!!@n @param  nprocs      number of subdomains
!
      module delete_data_files
!
      use m_precision
      use m_file_format_switch
!
      implicit none
!
!------------------------------------------------------------------
!
      contains
!
!------------------------------------------------------------------
!
      subroutine delete_file_if_exist(file_name)
!
      character(len=kchara), intent(in) :: file_name
!
!
      open(255, file=file_name, status='old', err=99)
      close(255, status='DELETE')
      write(*,*) trim(file_name), ' is deleted.'
!
  99  return
!
      end subroutine delete_file_if_exist
!
!------------------------------------------------------------------
!
      subroutine delete_file_by_f(file_name)
!
      character(len=kchara), intent(in) :: file_name
!
!
      open(255, file=file_name)
      close(255, status='DELETE')
      write(*,*) trim(file_name), ' is deleted.'
!
      end subroutine delete_file_by_f
!
!------------------------------------------------------------------
!
      subroutine delete_parallel_files(iflag_fmt, nprocs, file_head)
!
      use set_parallel_file_name
!
      integer(kind=kint), intent(in) :: iflag_fmt, nprocs
      character(len=kchara), intent(in) :: file_head
!
      integer :: ip, id_rank
      character(len=kchara) :: file_name, fname_tmp
!
!
      do ip = 1, int(nprocs)
        id_rank = ip - 1
        fname_tmp = add_process_id(id_rank, file_head)
!
        if(iflag_fmt .eq. id_gzip_txt_file_fmt) then
          file_name =  add_gzip_extension(fname_tmp)
        else
          file_name = fname_tmp
        end if
!
        call delete_file_by_f(file_name)
      end do
!
      end subroutine delete_parallel_files
!
!------------------------------------------------------------------
!------------------------------------------------------------------
!
      integer(kind = kint) function check_file_exist(file_name)
!
      use m_error_IDs
!
      character(len=kchara), intent(in) :: file_name
      integer(kind = kint), parameter :: id_file = 255
!
!
      open (id_file, file=file_name, status='old', err=99)
      close(id_file)
!
      check_file_exist = 0
      return
!
  99  continue
      check_file_exist = ierr_file
      return
!
      end function check_file_exist
!
!  ---------------------------------------------------------------------
!
      end module delete_data_files
