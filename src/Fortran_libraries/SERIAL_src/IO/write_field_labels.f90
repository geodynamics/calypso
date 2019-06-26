!>@file   write_field_labels.f90
!!@brief  module write_field_labels
!!
!!@author H. Matsui
!!@date Programmed in June, 2009
!
!>@brief  Write field labels in one line
!!
!!@verbatim
!!      integer(kind = kint) function                                   &
!!     &                    count_label_list_length(num, field_list)
!!      function make_field_list(length_tot, num, field_list)
!!        character(len = length_tot) :: make_field_list
!!      function append_to_field_list(len_org, list_org, len_add, added)
!!        character(len = len_org+len_add+4) :: make_field_list
!!
!!      subroutine write_one_label(id_file, label1)
!!      subroutine write_vector_label(id_file, label_v)
!!      subroutine write_sym_tensor_label(id_file, label_st)
!!
!!      subroutine write_two_labels(id_file, label1, label2)
!!      subroutine write_three_labels(id_file, label1, label2, label3)
!!      subroutine write_four_labels(id_file, label1, label2,           &
!!     &          label3, label4)
!!      subroutine write_six_labels(id_file, label1, label2,            &
!!     &          label3, label4, label5, label6)
!!      subroutine write_seven_labels(id_file, label1, label2,          &
!!     &          label3, label4, label5, label6, label7)
!!
!!      subroutine write_multi_labels(id_file, nlabel, labels)
!!      subroutine write_one_label_cont(id_file, label1)
!!@endverbatim
!!
!
      module write_field_labels
!
      use m_precision
      use m_constants
!
      implicit none
!
! ----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      integer(kind = kint) function                                     &
     &                    count_label_list_length(num, field_list)
!
      integer(kind = kint), intent(in) :: num
      character(len = kchara), intent(in) :: field_list(num)
!
      integer(kind = kint) :: i
!
!
      count_label_list_length = 0
      do i = 1, num
        count_label_list_length                                         &
     &        = count_label_list_length + len_trim(field_list(i)) + 4
      end do
!
      end function count_label_list_length
!
! -----------------------------------------------------------------------
!
      function make_field_list(length_tot, num, field_list)
!
      integer(kind = kint), intent(in) :: length_tot
      integer(kind = kint), intent(in) :: num
      character(len = kchara), intent(in) :: field_list(num)
!
      character(len = length_tot) :: make_field_list
!
      integer(kind = kint) :: i
      character(len=kchara) :: fmt_txt
!
!
      if(num .le. 0) then
        make_field_list = ''
      else
        write(fmt_txt,'(a1,i4,a10)')    '(', num, '(a,4X),a1)'
        write(make_field_list,fmt_txt) (trim(field_list(i)),i=1,num)
      end if
!
      end function make_field_list
!
! -----------------------------------------------------------------------
!
      function append_to_field_list(len_org, list_org, len_add, added)
!
      integer(kind = kint), intent(in) :: len_org, len_add
      character(len = len_org), intent(in) :: list_org
      character(len = len_add), intent(in) :: added
!
      character(len = len_org+len_add+4) :: append_to_field_list
!
!
      append_to_field_list = list_org // '    ' // added
!
      end function append_to_field_list
!
! -----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_one_label(id_file, label1)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label1
!
      call write_one_label_cont(id_file, label1)
!
      end subroutine write_one_label
!
! ----------------------------------------------------------------------
!
      subroutine write_vector_label(id_file, label_v)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label_v(3)
!
!
      call write_multi_labels(id_file, ithree, label_v)
!
      end subroutine write_vector_label
!
! ----------------------------------------------------------------------
!
      subroutine write_sym_tensor_label(id_file, label_st)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label_st(6)
!
!
      call write_multi_labels(id_file, isix, label_st)
!
      end subroutine write_sym_tensor_label
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_two_labels(id_file, label1, label2)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label1, label2
!
      call write_one_label_cont(id_file, label1)
      call write_one_label_cont(id_file, label2)
!
      end subroutine write_two_labels
!
! ----------------------------------------------------------------------
!
      subroutine write_three_labels(id_file, label1, label2, label3)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label1, label2, label3
!
      call write_one_label_cont(id_file, label1)
      call write_one_label_cont(id_file, label2)
      call write_one_label_cont(id_file, label3)
!
      end subroutine write_three_labels
!
! ----------------------------------------------------------------------
!
      subroutine write_four_labels(id_file, label1, label2,             &
     &          label3, label4)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label1, label2
      character(len=kchara), intent(in) :: label3, label4
!
      call write_one_label_cont(id_file, label1)
      call write_one_label_cont(id_file, label2)
      call write_one_label_cont(id_file, label3)
      call write_one_label_cont(id_file, label4)
!
      end subroutine write_four_labels
!
! ----------------------------------------------------------------------
!
      subroutine write_six_labels(id_file, label1, label2,              &
     &          label3, label4, label5, label6)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label1, label2, label3
      character(len=kchara), intent(in) :: label4, label5, label6
!
      call write_one_label_cont(id_file, label1)
      call write_one_label_cont(id_file, label2)
      call write_one_label_cont(id_file, label3)
      call write_one_label_cont(id_file, label4)
      call write_one_label_cont(id_file, label5)
      call write_one_label_cont(id_file, label6)
!
      end subroutine write_six_labels
!
! ----------------------------------------------------------------------
!
      subroutine write_multi_labels(id_file, nlabel, labels)
!
      integer(kind = kint), intent(in) :: id_file, nlabel
      character(len=kchara), intent(in) :: labels(nlabel)
!
      integer(kind = kint) :: nd
!
      do nd = 1, nlabel
        call write_one_label_cont(id_file, labels(nd) )
      end do
!
      end subroutine write_multi_labels
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_seven_labels(id_file, label1, label2,            &
     &          label3, label4, label5, label6, label7)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label1, label2
      character(len=kchara), intent(in) :: label3, label4
      character(len=kchara), intent(in) :: label5, label6, label7
!
      call write_one_label_cont(id_file, label1)
      call write_one_label_cont(id_file, label2)
      call write_one_label_cont(id_file, label3)
      call write_one_label_cont(id_file, label4)
      call write_one_label_cont(id_file, label5)
      call write_one_label_cont(id_file, label6)
      call write_one_label_cont(id_file, label7)
!
      end subroutine write_seven_labels
!
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
!
      subroutine write_one_label_cont(id_file, label1)
!
      integer(kind = kint), intent(in) :: id_file
      character(len=kchara), intent(in) :: label1
!
      write(id_file,'(2a)',advance='no') trim(label1), '    '
!
      end subroutine write_one_label_cont
!
! ----------------------------------------------------------------------
!
      end module write_field_labels
