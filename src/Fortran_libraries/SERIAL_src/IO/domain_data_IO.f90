!
!      module domain_data_IO
!
!     Written by H. Matsui on July, 2007
!
!      subroutine read_domain_info(id_file)
!      subroutine read_domain_info_b(id_file)
!      subroutine write_domain_info(id_file)
!      subroutine write_domain_info_b(id_file)
!
      module domain_data_IO
!
      use m_precision
!
      use m_comm_data_IO
!
      implicit none
!
      character(len=255) :: character_4_read = ''
      private :: character_4_read
!
!------------------------------------------------------------------
!
       contains
!
!------------------------------------------------------------------
!
       subroutine read_domain_info(id_file)
!
       use skip_comment_f
!
       integer(kind = kint), intent(in) :: id_file
!
!
       call skip_comment(character_4_read,id_file)
       read(character_4_read,*) my_rank_IO
!
       read(id_file,*) num_neib_domain_IO
!
       call allocate_neib_domain_IO
!
       if (num_neib_domain_IO .gt. 0) then
         read(id_file,*) id_neib_domain_IO(1:num_neib_domain_IO)
       end if
!
       end subroutine read_domain_info
!
!------------------------------------------------------------------
!
       subroutine read_domain_info_b(id_file)
!
       integer(kind = kint), intent(in) :: id_file
!
!
       read(id_file) my_rank_IO
       read(id_file) num_neib_domain_IO
       call allocate_neib_domain_IO
!
       if (num_neib_domain_IO .gt. 0) then
         read(id_file) id_neib_domain_IO(1:num_neib_domain_IO)
       end if
!
       end subroutine read_domain_info_b
!
!------------------------------------------------------------------
!
      subroutine write_domain_info(id_file)
!
       integer(kind = kint), intent(in) :: id_file
!
!
!      write(id_file,'(a)') '! '
!      write(id_file,'(a)') '! 1.parallel information'
!      write(id_file,'(a)') '!    domain ID'
!      write(id_file,'(a)') '!    number of domain for transfer'
!      write(id_file,'(a)') '!    domain ID for transfer'
!      write(id_file,'(a)') '! '
!
      write(id_file,'(i16)') my_rank_IO
      write(id_file,'(i16)') num_neib_domain_IO
!
      if (num_neib_domain_IO .gt. 0) then
        write(id_file,'(8i16)') id_neib_domain_IO(1:num_neib_domain_IO)
      else
        write(id_file,'(a)') ''
      end if
!
      call deallocate_neib_domain_IO
!
      end subroutine write_domain_info
!
!------------------------------------------------------------------
!
      subroutine write_domain_info_b(id_file)
!
       integer(kind = kint), intent(in) :: id_file
!
!
      write(id_file) my_rank_IO
      write(id_file) num_neib_domain_IO
!
      if (num_neib_domain_IO .gt. 0) then
        write(id_file) id_neib_domain_IO(1:num_neib_domain_IO)
      end if
!
      call deallocate_neib_domain_IO
!
      end subroutine write_domain_info_b
!
!------------------------------------------------------------------
!
      end module domain_data_IO
