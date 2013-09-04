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
!!      subroutine delete_file_by_f(file_name)
!!      subroutine delete_parallel_files(iflag_fmt, nprocs, file_head)
!!@endverbatim
!!
!!@n @param  file_name   file name to delete
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
      integer(kind = kint) :: ip, my_rank
      character(len=kchara) :: file_name, fname_tmp
!
!
      do ip = 1, nprocs
        my_rank = ip - 1
        call add_int_suffix(my_rank, file_head, fname_tmp)
!
        if(iflag_fmt .eq. id_gzip_txt_file_fmt) then
          call add_gzip_extension(fname_tmp, file_name)
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
!
      end module delete_data_files
