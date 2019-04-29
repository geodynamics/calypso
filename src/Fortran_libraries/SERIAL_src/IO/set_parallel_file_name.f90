!>@file   set_parallel_file_name.f90
!!@brief  module set_parallel_file_name
!!
!!@author H. Matsui
!!@date   Programmed  H. Matsui in 2004
!
!>@brief  Set file extension
!!
!!@verbatim
!!      character(len = kchara) function delete_directory_name          &
!!     &                               (dir_file_name)
!!
!!      character(len = kchara) function add_process_id                 &
!!     &                               (id_rank, file_head)
!!      character(len = kchara) function add_int_suffix                 &
!!     &                               (int_id, file_head)
!!
!!      character(len = kchara) function add_dat_extension(file_head)
!!                put ".dat" at the end
!!
!!      character(len = kchara) function add_elaps_postfix(file_head)
!!                put ".elps" at the end
!!
!!      character(len = kchara) function add_gzip_extension(file_head)
!!                put ".gz"
!!      character(len = kchara) function add_null_character(file_head)
!!                put null character at the end
!!
!!      character(len=kchara) function                                  &
!!     &                     add_2chara_extension(file_head, extension)
!!      character(len=kchara) function                                  &
!!     &                     add_3chara_extension(file_head, extension)
!!
!!       character(len=kchara) function add_left_label(file_head)
!!                put "_left" at the end
!!       character(len=kchara) function add_right_label(file_head)
!!                put "_right" at the end
!!
!!      subroutine add_index_after_name(int_id, chara_head, chara_name)
!!      subroutine int_to_str(int_val, int_string)
!!      subroutine lint_to_str(lint_val, int_string)
!!      subroutine real_to_str(real_val, real_string)
!!
!!      character(len = kchara) function set_rayleigh_file_name         &
!!     &                               (dir, int_id, postfix)
!!               set file name as "[dir]/[int_id]_[postfix]"
!!@endverbatim
!!
!!@n @param dir_file_name    file name (header) including directory name
!!@n @param int_id           integer to be added at the end of prefix
!!@n @param file_name        output file name
!!@n @param int_val          integer to be tranfered to character
!!@n @param int_string       output character
!
      module set_parallel_file_name
!
      use m_precision
!
      implicit  none
!
      character(len=2), parameter, private :: gz_ext =  "gz"
!
!-----------------------------------------------------------------------
!
      contains
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function delete_directory_name            &
     &                               (dir_file_name)
!
      character(len=kchara), intent(in) :: dir_file_name
!
      integer(kind = kint) :: i, len, len_name, idir_flag
      character(len=kchara) :: fmt_txt
!
!
      len_name = len_trim(dir_file_name)
      idir_flag = 0
      do i = len_name, 1, -1
        if(dir_file_name(i:i) .eq. '/') then
          idir_flag = i
          exit
        end if
      end do
      len = len_name - idir_flag
!
      write(fmt_txt,'(a2,i4,a1)') '(a',len,')'
      write(delete_directory_name,fmt_txt)                              &
     &                            dir_file_name(idir_flag+1:len_name)
!
      end function delete_directory_name
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_process_id                   &
     &                               (id_rank, file_head)
!
      integer, intent(in) :: id_rank
      character(len=kchara), intent(in) :: file_head
!
      integer(kind = kint) :: int_id
!
!
      int_id = int(id_rank, KIND(int_id))
      add_process_id = add_int_suffix(int_id, file_head)
!
      end function add_process_id
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_int_suffix                   &
     &                               (int_id, file_head)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: file_head
!
      character(len=kchara) :: chara_head
!
!
      write(chara_head,1000) trim(file_head)
      call add_index_after_name(int_id, chara_head, add_int_suffix)
!
 1000 format (a,".")
!
      end function add_int_suffix
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_dat_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
       write(add_dat_extension,1011) trim(file_head)
 1011 format (a,".dat")
!
      end function add_dat_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_elaps_postfix(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
       write(add_elaps_postfix,1011) trim(file_head)
 1011 format (a,".elps")
!
      end function add_elaps_postfix
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_gzip_extension(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      add_gzip_extension = add_2chara_extension(file_head, gz_ext)
!
      end function add_gzip_extension
!
!-----------------------------------------------------------------------
!
      character(len = kchara) function add_null_character(file_head)
!
      character(len=kchara), intent(in) :: file_head
!
      write(add_null_character,1011) trim(file_head), char(0)
 1011 format (a,a1)
!
      end function add_null_character
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &                     add_2chara_extension(file_head, extension)
!
      character(len=kchara), intent(in) :: file_head
      character(len=2), intent(in) :: extension
!
       write(add_2chara_extension,'(a,a1,a2)')                          &
     &                   trim(file_head), '.', trim(extension)
!
      end function add_2chara_extension
!
!-----------------------------------------------------------------------
!
      character(len=kchara) function                                    &
     &                     add_3chara_extension(file_head, extension)
!
      character(len=kchara), intent(in) :: file_head
      character(len=3), intent(in) :: extension
!
       write(add_3chara_extension,'(a,a1,a3)')                          &
     &                   trim(file_head), '.', trim(extension)
!
      end function add_3chara_extension
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
       character(len=kchara) function add_left_label(file_head)
!
      character(len=kchara), intent(in) :: file_head
      character(len=kchara) :: file_name
!
       write(file_name,1011) trim(file_head)
       add_left_label = trim(file_name)
 1011 format (a,"_left")
!
      end function add_left_label
!
!-----------------------------------------------------------------------
!
       character(len=kchara) function add_right_label(file_head)
!
      character(len=kchara), intent(in) :: file_head
      character(len=kchara) :: file_name
!
       write(file_name,1011) trim(file_head)
       add_right_label = trim(file_name)
 1011 format (a,"_right")
!
      end function add_right_label
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine add_index_after_name(int_id, chara_head, chara_name)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: chara_head
      character(len=kchara), intent(inout) :: chara_name
!
      character(len=kchara) :: charaint
!
!
      write(charaint,*) int_id
      write(chara_name,'(a,a)')                                         &
                trim(chara_head), trim(ADJUSTL(charaint))
!
      end subroutine add_index_after_name
!
!-----------------------------------------------------------------------
!
      subroutine int_to_str(int_val, int_string)
!
      integer(kind=kint), intent(in) :: int_val
      character(len=kchara), intent(inout) :: int_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) int_val
      write(int_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine int_to_str
!
!-----------------------------------------------------------------------
!
      subroutine lint_to_str(lint_val, int_string)
!
      integer(kind=kint_gl), intent(in) :: lint_val
      character(len=kchara), intent(inout) :: int_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) lint_val
      write(int_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine lint_to_str
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      subroutine real_to_str(real_val, real_string)
!
      real(kind=kreal), intent(in) :: real_val
      character(len=kchara), intent(inout) :: real_string
      character(len=kchara) :: tmp_string
!
!
      write(tmp_string,*) real_val
      write(real_string,'(a)') trim(adjustl(tmp_string))
!
      end subroutine real_to_str
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
      character(len = kchara) function set_rayleigh_file_name           &
     &                               (dir, int_id, postfix)
!
      integer(kind = kint), intent(in) :: int_id
      character(len=kchara), intent(in) :: dir, postfix
!
!
      write(set_rayleigh_file_name,1000)                                &
     &                        trim(dir), int_id, trim(postfix)
!
 1000 format(a, '/', i8.8, '_', a)
!
      end function set_rayleigh_file_name
!
!-----------------------------------------------------------------------
!
      end module set_parallel_file_name
