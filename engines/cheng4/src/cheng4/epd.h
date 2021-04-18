/*
You can use this program under the terms of either the following zlib-compatible license
or as public domain (where applicable)

  Copyright (C) 2012-2015 Martin Sedlak

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgement in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

#pragma once

#include "move.h"
#include <string>
#include <vector>

namespace cheng4
{

struct EPDPosition
{
	std::string fen;
	std::vector< cheng4::Move > avoid;
	std::vector< cheng4::Move > best;
};

class EPDFile
{
public:
	std::vector< EPDPosition > positions;

	// load EPD file
	bool load( const char *fnm );

	// parse from zero-terminated buffer
	bool parse( const char *ptr );
};

}
