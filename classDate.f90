module class_Date
!=================================================================================
!   How to Use:
!       use class_Date
!       Type(Date)::    Today, Tomorrow...
!=================================================================================
	implicit none
	public::	   	Date,       operator(+),    operator(-),                     &
					elapseDay,  setDate,        readDate,       printDate,		 &
					writeDate,  getDateDMY,     getJulianDay
	type Date
		private
		integer::	day,    month,  year
	end type Date
	private
	integer::		days(12,0:1),  cumdays(12,0:1)
	!leap year    			  1  2  3   4   5   6   7   8   9  10  11  12  1  2  3   4   5   6   7   8   9  10  11  12
	data			days    /31,28,31, 30, 31, 30, 31, 31, 30, 31, 30, 31,31,29,31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
	data			cumdays /31,59,90,120,151,181,212,243,273,304,334,365,31,60,91,121,152,182,213,244,274,305,335,366/
	character(3)::	mName(13)
	data          	mName   /"Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","non"/
!=================================================================================
   interface operator(+)
		module procedure  AddDate
   end interface
   interface operator(-)
	   module procedure  SubDate
   end interface
!=================================================================================
	contains
		function Date_(d, m, y) result (ObjDate)  !public constructor
			integer, intent(in)::			d, m, y
			type (Date):: 					ObjDate
			integer::						mon, day
			mon = m ; day = d
			if (mon < 1 .or. mon > 12) mon = 13
			if (day < 1 .or. day > 31) day =  0
			ObjDate = Date (day, mon, y)
		end function Date_
		function LeapYear(x) result(y)
			integer, intent(in)::x
			integer::				y
			if ((mod(x,4) == 0 .and. mod(x,100) /= 0) .or. (mod(x,400) == 0)) then
			    y = 1
			else
			    y = 0
			endif
		end function
		function AddDate(ObjDate,y) result(SujDate)
			type(Date), intent(in)::		ObjDate
			integer, intent(in)::			y    !days to be added
			type(Date)::					SujDate
			!if (y < 0) return   !SubDate(ObjDate, -y)
			SujDate%day   = ObjDate%day + y
			SujDate%month = ObjDate%month
			SujDate%year  = ObjDate%year
			do while (SujDate%day > days(SujDate%month,LeapYear(SujDate%year)))
				SujDate%day   = SujDate%day - days(SujDate%month,LeapYear(SujDate%year))
				SujDate%month = SujDate%month + 1
				if (SujDate%month > 12) then
					SujDate%year  = SujDate%year + 1
				   SujDate%month = 1
				endif
			enddo
		end function
		function SubDate(ObjDate,y) result(SujDate)
			type(Date), intent(in)::		ObjDate
			integer, intent(in)::			y    !days to be subtructed
			type(Date)::					SujDate
			!if (y < 0) return   !AddDate(ObjDate, -y)
			SujDate%day   = ObjDate%day - y
			SujDate%month = ObjDate%month
			SujDate%year  = ObjDate%year
			do while (SujDate%day < 1)
				SujDate%month = SujDate%month - 1
				if (SujDate%month < 1) then
					SujDate%year  = SujDate%year - 1
					SujDate%month = 12
				endif
				SujDate%day = SujDate%day + days(SujDate%month,LeapYear(SujDate%year))
			enddo
		end function
		subroutine getDateDMY(ObjDate, d, m, y)
			type(Date), intent(in)::		ObjDate
			integer, intent(out)::			d,  m,  y
			d = ObjDate%day
			m = ObjDate%month
			y = ObjDate%year
		end subroutine getDateDMY
		subroutine readDate(Input,ObjDate)
			type(Date), intent(out)::		ObjDate
			integer,intent(in)::			Input
			read (Input,*) ObjDate%day, ObjDate%month, ObjDate%year
		end subroutine readDate
		subroutine writeDate(Output,ObjDate)
			type(Date), intent(in)::		ObjDate
			integer,intent(in)::	   		Output
			if (ObjDate%month < 1 .or. ObjDate%month > 12) then
				write(5, *) "Invalid month"
			else if (ObjDate%day   < 1 .or. ObjDate%day   > 31) then
				write(Output, *) "Invalid day  "
			else
				write(Output, '(i2,"-",a3,"-",i4)', advance="no") ObjDate%day,trim(mName(ObjDate%month)), ObjDate%year
			endif
		end subroutine writeDate
		subroutine printDate(ObjDate)
			type(Date), intent(in)::		ObjDate
			if (ObjDate%month < 1 .or. ObjDate%month > 12) then
				print *, "Invalid month"
			else if (ObjDate%day   < 1 .or. ObjDate%day   > 31) then
				print *, "Invalid day  "
			else
				print '(i2," - ",a3," - ",i4)', ObjDate%day,trim(mName(ObjDate%month)), ObjDate%year
			endif
		end subroutine printDate
		function today_is() result (ObjDate)
			type(Date)::					ObjDate
			integer:: 						y, m, d
			character(8)::          		today
			call date_and_time (DATE=today)
			read (today(1:4), '(i4)') y
			read (today(5:6), '(i2)') m
			read (today(7:8), '(i2)') d
			ObjDate = Date(m, d, y)
		end function today_is
		function setDate(d, m, y) result (ObjDate)
			integer, optional, intent(in):: m, d, y
			type(Date)::					ObjDate
			ObjDate = Date (1,1,1970)     !Unix Time
			if (present(d)) ObjDate%day   = d
			if (present(m)) ObjDate%month = m
			if (present(y)) ObjDate%year  = y
		end function setDate
		function getJulianDay(objDate) result (d)
		! Julian date (1970,1,1) -> 2440588
			integer::                  		d
			type(Date), intent(in)::		objDate
			d = objDate%day - 32075                                              &
			  + 1461 *  (objDate%year  + 4800 + (objDate%month-14)/12     )/ 4   &
			  +  367 *  (objDate%month - 2 -   ((objDate%month-14)/12)*12 )/12   &
			  -    3 *( (objDate%year  + 4900 + (objDate%month-14)/12)/100 )/4
		end function getJulianDay
		function getCalendarDay(objDate) result (d)
			integer::                  		d
			type(Date), intent(in)::		objDate
			d = 365 * objDate%year + cumdays(objDate%month,LeapYear(objDate%year)) + objDate%day
		end function getCalendarDay
		function elapseDay(Date1, Date2)
			integer::						elapseDay
			type(Date), intent(in)::		Date1, Date2
			elapseDay = getCalendarDay(Date1) - getCalendarDay(Date2)
!			elapseDay = getJulianDay(Date1) - getJulianDay(Date2)
		end function elapseDay
!=================================================================================
end module class_Date
