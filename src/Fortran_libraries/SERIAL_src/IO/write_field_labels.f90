!>@file   write_field_labels.f90
!!@brief  module write_field_labels
!!
!!@author H. Matsui
!!@date Programmed in June, 2009
!
!>@brief  Write field labels in one line
!!
!!@verbatim
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
!
      private :: write_one_label_cont
!
! ----------------------------------------------------------------------
!
      contains
!
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
