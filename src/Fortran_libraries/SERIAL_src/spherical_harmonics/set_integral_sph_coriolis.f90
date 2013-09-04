!set_integral_sph_coriolis.f90
!      module set_integral_sph_coriolis
!
!     Written by H. Matsui on July, 2007
!
!      subroutine s_set_integral_sph_coriolis(ltr, jmax, idx_j)
!
      module set_integral_sph_coriolis
!
      use m_precision
!
      implicit none
!
! -----------------------------------------------------------------------
!
      contains
!
! -----------------------------------------------------------------------
!
      subroutine s_set_integral_sph_coriolis(ltr, jmax, idx_j)
!
      use m_constants
      use m_integrals_4_sph_coriolis
      use cal_gaunt_itgs
!
      integer(kind = kint), intent(in) :: ltr, jmax
      integer(kind = kint), intent(in) :: idx_j(jmax,3)
!
      integer(kind = kint) :: jnod, j, l, m, l2, m2, i
!
!
      write(*,*) "Integration data for Omega=(0,0,1)"
      write(*,'(a17)',advance='NO') 'Loop start       '
      do jnod = 1, jmax
        j = idx_j(jnod,1)
        l = idx_j(jnod,2)
        m = idx_j(jnod,3)
        if(l.eq.0) cycle
!
!   omega(l,m) = (1,0)
!    Ki
        l2 = l - 1
        m2 = m
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2).le.l2) then
          gk_cor(jnod,1,2) = leadki(ione, izero, l2, m2, l, m)
          jgl_kcor(jnod,1,2) = l2*(l2+ione) + m2
        end if
!
        l2 = l + 1
        m2 = m
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2).le.l2) then
          gk_cor(jnod,2,2) = leadki(ione, izero, l2, m2, l, m)
          jgl_kcor(jnod,2,2) = l2*(l2+ione) + m2
        end if
!
!   Li
        l2 =  l
        m2 = -m
        el_cor(jnod,1,2) = leadli(ione, izero, l2, m2, l, m)
        jgl_lcor(jnod,1,2) = l2*(l2+ione) + m2
!
        if(l .eq. m) then
          write(*,'(17a1)',advance='NO') (char(8),i=1,17)
          write(*,'(a,i6,a)',advance='NO') 'Degree ', l, ' end'
        end if
      end do
      write(*,*) ''
!
!   l1= 1, m1 = -1
!    Ki
      write(*,*) "Integration data for Omega=(0,1,0)"
      write(*,'(a17)',advance='NO') 'Loop start       '
      do jnod = 1, jmax
        j = idx_j(jnod,1)
        l = idx_j(jnod,2)
        m = idx_j(jnod,3)
        if(l.eq.0) cycle
!
        l2 =  l - 1
        m2 = -m - 1
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2) .le. l2) then
          gk_cor(jnod,1,1) = leadki(ione, -ione, l2, m2, l, m)
          jgl_kcor(jnod,1,1) = l2*(l2+ione) + m2
        end if
!
        l2 =  l - 1
        m2 = -m + 1
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2) .le. l2) then
          gk_cor(jnod,2,1) = leadki(ione, -ione, l2, m2, l, m)
          jgl_kcor(jnod,2,1) = l2*(l2+ione) + m2
        end if
!
        l2 =  l + 1
        m2 = -m - 1
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2) .le. l2) then
          gk_cor(jnod,3,1) = leadki(ione, -ione, l2, m2, l, m)
          jgl_kcor(jnod,3,1) = l2*(l2+ione) + m2
        end if
!
        l2 =  l + 1
        m2 = -m + 1
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2) .le. l2) then
          gk_cor(jnod,4,1) = leadki(ione, -ione, l2, m2, l, m)
          jgl_kcor(jnod,4,1) = l2*(l2+ione) + m2
        end if
!
!  Li
        m2 = m - 1
        if(abs(m2) .le. l) then
          el_cor(jnod,1,1) = leadli(ione, -ione, l, m2, l, m)
          jgl_lcor(jnod,1,1) = l*(l+ione) + m2
        end if
!
        m2 = m + 1
        if(abs(m2) .le. l) then
          el_cor(jnod,2,1) = leadli(ione, -ione, l, m2, l, m)
          jgl_lcor(jnod,2,1) = l*(l+ione) + m2
        end if
!*
        if(l .eq. m) then
          write(*,'(17a1)',advance='NO') (char(8),i=1,17)
          write(*,'(a,i6,a)',advance='NO') 'Degree ', l, ' end'
        end if
      end do
      write(*,*) ''
!
!    l1 = 1, m1 = 1
      write(*,*) "Integration data for Omega=(1,0,0)"
      write(*,'(a17)',advance='NO') 'Loop start       '
      do jnod = 1, jmax
        j = idx_j(jnod,1)
        l = idx_j(jnod,2)
        m = idx_j(jnod,3)
        if(l.eq.0) cycle
!
!       Ki
        l2 = l - 1
        m2 = m - 1
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2).le.l2) then
          gk_cor(jnod,1,3) = leadki(ione, ione, l2, m2, l, m)
          jgl_kcor(jnod,1,3) = l2*(l2+ione) + m2
        end if
!
        l2 = l - 1
        m2 = m + 1
        if(l2 .ge. 1 .and. l2.le.ltr .and. abs(m2).le.l2) then
          gk_cor(jnod,2,3) = leadki(ione, ione, l2, m2, l, m)
          jgl_kcor(jnod,2,3) = l2*(l2+ione) + m2
        end if
!
        l2 = l + 1
        m2 = m - 1
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2).le.l2) then
          gk_cor(jnod,3,3) = leadki(ione, ione, l2, m2, l, m)
          jgl_kcor(jnod,3,3) = l2*(l2+ione) + m2
        end if
!
        l2 = l + 1
        m2 = m + 1
        if(l2.ge.1 .and. l2.le.ltr .and. abs(m2).le.l2) then
          gk_cor(jnod,4,3) = leadki(ione, ione, l2, m2, l, m)
          jgl_kcor(jnod,4,3) = l2*(l2+ione) + m2
        end if
!
!    Li
        m2 = -m-1
        if(abs(m2) .le. l) then
          el_cor(jnod,1,3) = leadli(ione, ione, l, m2, l, m)
          jgl_lcor(jnod,1,3) = l*(l+ione) + m2
        end if
!
        m2 = -m+1
        if(abs(m2) .le. l) then
          el_cor(jnod,2,3) = leadli(ione, ione, l, m2, l, m)
          jgl_lcor(jnod,2,3) = l*(l+ione) + m2
        end if
!
        if(l .eq. m) then
          write(*,'(17a1)',advance='NO') (char(8),i=1,17)
          write(*,'(a,i6,a)',advance='NO') 'Degree ', l, ' end'
        end if
      end do
      write(*,*) ''
!
      end subroutine s_set_integral_sph_coriolis
!
! -----------------------------------------------------------------------
!
      end module set_integral_sph_coriolis
